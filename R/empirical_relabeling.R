#' Ask an LLM to label one or more sets of items
#'
#' Sends the items of one or more scales to an LLM (through `ellmer`) and asks
#' for a short label (2 to 4 words) and a definition for each set, based only
#' on the item content.
#'
#' With one item set (e.g. the items of two merged jangle scales) the model
#' names the construct the items measure. With two or more sets (e.g. a jingle
#' pair) the model writes a label for each set that is close to its own items
#' and clearly different from the other set(s). `current_labels` tells the
#' model which labels were too similar.
#'
#' @param item_sets List of character vectors, one per scale, with the item
#'   texts.
#' @param current_labels Optional character vector, one per item set, with the
#'   current label (and definition) of each scale. Used for the contrast
#'   prompt.
#' @param provider One of `"openai"`, `"anthropic"`, `"google"`,
#'   `"deepseek"`. Selects the `ellmer::chat_*()` constructor.
#' @param model Model name passed to the provider, e.g. `"gpt-4.1-mini"`,
#'   `"claude-sonnet-4-5"`, `"gemini-2.5-flash"`.
#' @param chat_args Named list of extra arguments for the `ellmer::chat_*()`
#'   constructor, e.g. `list(params = ellmer::params(temperature = 0))`.
#' @param min_words,max_words Suggested length of the definition.
#' @param template Optional definition template (see
#'   [harmonize_definitions()]). When given, the definitions follow it and the
#'   template's own length and wording rules replace `min_words`/`max_words`.
#' @param cache_dir Optional folder. When given, each answer is saved there and
#'   reused when the same prompt is sent to the same model again.
#' @param max_retries Number of attempts before giving up.
#'
#' @return Tibble with one row per item set: `set`, `label`, `definition`.
#'   The prompt is stored in the `"prompt"` attribute.
#'
#' @examples
#' \dontrun{
#' llm_label_items(
#'   list(c("Is fascinated by art, music, or literature",
#'          "Values art and beauty")),
#'   provider = "openai", model = "gpt-4.1-mini"
#' )
#' }
#' @export
llm_label_items <- function(
    item_sets,
    current_labels = NULL,
    provider = c("openai", "anthropic", "google", "deepseek"),
    model = "gpt-4.1-mini",
    chat_args = list(),
    min_words = 12,
    max_words = 60,
    template = NULL,
    cache_dir = NULL,
    max_retries = 3) {

  provider <- match.arg(provider)
  if (!is.list(item_sets) || length(item_sets) == 0) {
    stop("`item_sets` must be a non-empty list of character vectors.", call. = FALSE)
  }

  prompt <- embeddcv_label_prompt(item_sets, current_labels, min_words, max_words, template)

  # one field per item set, so the model must return exactly one label each
  scale_type <- ellmer::type_object(
    label = ellmer::type_string("Short label, 2 to 4 words."),
    definition = ellmer::type_string("Definition of the construct.")
  )
  type <- do.call(ellmer::type_object, stats::setNames(
    rep(list(scale_type), length(item_sets)), paste0("scale_", seq_along(item_sets))))

  res <- embeddcv_llm_structured(prompt, type, provider, model, chat_args,
                                 cache_dir, max_retries)
  if (is.data.frame(res)) return(res)   # cache written by embeddcv <= 0.2.0

  out <- dplyr::tibble(
    set = seq_along(item_sets),
    label = unname(vapply(res, function(r) as.character(r$label), character(1))),
    definition = unname(vapply(res, function(r) as.character(r$definition), character(1)))
  )
  attr(out, "prompt") <- prompt
  out
}

# One structured LLM call through ellmer, with retries and an optional cache
# keyed by prompt, schema, provider, model and chat_args.
embeddcv_llm_structured <- function(prompt, type, provider, model, chat_args = list(),
                                    cache_dir = NULL, max_retries = 3) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package `ellmer` is required. Install it with install.packages(\"ellmer\").",
         call. = FALSE)
  }

  cache_file <- NULL
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    key <- rlang::hash(list(prompt, provider, model, chat_args))
    cache_file <- file.path(cache_dir, paste0("llm_", key, ".rds"))
    if (file.exists(cache_file)) return(readRDS(cache_file))
  }

  constructor <- switch(provider,
    openai    = ellmer::chat_openai,
    anthropic = ellmer::chat_anthropic,
    google    = ellmer::chat_google_gemini,
    deepseek  = ellmer::chat_deepseek,
    stop("Unknown provider: ", provider, call. = FALSE)
  )
  chat <- do.call(constructor, c(list(model = model, echo = "none"), chat_args))

  res <- NULL
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(
      chat$clone()$chat_structured(prompt, type = type),
      error = function(e) {
        message("LLM call failed (attempt ", attempt, "): ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(res)) break
    Sys.sleep(2^attempt)
  }
  if (is.null(res)) {
    stop("The LLM call failed after ", max_retries, " attempts.", call. = FALSE)
  }
  if (!is.null(cache_file)) saveRDS(res, cache_file)
  res
}

embeddcv_label_prompt <- function(item_sets, current_labels, min_words, max_words,
                                  template = NULL) {
  k <- length(item_sets)

  task <- if (k == 1) {
    paste0(
      "Below are the items of a psychological scale. Write a short label ",
      "(2 to 4 words) and a definition of the construct these items measure."
    )
  } else {
    paste0(
      "Below are the items of ", k, " psychological scales. Their current labels ",
      "are too similar, but their items measure different content. For each ",
      "scale, write a new short label (2 to 4 words) and a definition that ",
      "describe its own items closely and clearly distinguish it from the ",
      "other scale", if (k > 2) "s", ". Answer scale_1 for Scale 1, scale_2 for ",
      "Scale 2, and so on."
    )
  }

  rules <- paste0(
    "Rules:\n",
    "- Base the label and definition only on the content of the items.\n",
    "- Some items are reverse-keyed (they describe the opposite pole); name ",
    "the construct by its positive pole.\n",
    if (is.null(template)) paste0(
      "- The definition should have ", min_words, " to ", max_words, " words; ",
      "use more only if the content really needs it.\n",
      "- Do not open the definition with generic phrases such as 'Tendency to' ",
      "or 'Propensity to'; state the distinctive content directly.\n"),
    "- Do not repeat the label in the definition, and do not mention the ",
    "items or the scale itself."
  )
  if (!is.null(template)) {
    rules <- paste0(rules, "\n- Write the definition following this template:\n", template)
  }

  blocks <- vapply(seq_len(k), function(i) {
    head <- if (k == 1) "Items:" else paste0("Scale ", i, ":")
    if (!is.null(current_labels)) {
      head <- paste0(head, "\nCurrent label: ", current_labels[[i]], "\nItems:")
    }
    paste0(head, "\n", paste0("- ", item_sets[[i]], collapse = "\n"))
  }, character(1))

  paste(task, rules, paste(blocks, collapse = "\n\n"), sep = "\n\n")
}

#' Iterative empirical relabeling of scales
#'
#' Repeats jingle-jangle classification and LLM relabeling for `n_steps`
#' steps. In each step:
#'
#' 1. Scale embeddings are the mean of the item embeddings of each active
#'    scale; label embeddings are the embeddings of each scale's current
#'    definition.
#' 2. Pairs are classified as jingle or jangle fallacies with cutoffs at the
#'    `probs` quantiles of the scale-scale and label-label similarities of
#'    that step.
#' 3. **Jangle** pairs (similar items, different definitions) are merged:
#'    their items are combined into one new scale, and the LLM writes its
#'    label and definition from the combined items.
#' 4. **Jingle** pairs (different items, similar definitions) are relabeled
#'    (contrasted): the LLM writes new labels and definitions for both
#'    scales, closer to their items and more distinct from each other. A pair
#'    is contrasted at most `jingle_patience` times; if it is still a jingle
#'    after that, it is reported as unresolved instead of being sent back to
#'    the LLM.
#' 5. The new definitions are embedded and the next step starts.
#'
#' A scale enters at most one action per step: pairs are taken in order of
#' the largest gap between scale and label similarity, skipping pairs whose
#' scales were already used, and scales merged in a step are not also
#' contrasted in that step. The loop stops early when there is nothing left
#' to act on: no jangle pairs and no jingle pairs below their patience.
#'
#' @param items Data frame with one row per item: `item_id`, `item_text` and
#'   `scale_id` (the original scale).
#' @param scales Data frame with one row per original scale: `scale_id`,
#'   `label` (short display label) and `definition`.
#' @param item_emb Item embedding matrix; row names are `item_id`.
#' @param def_emb Optional definition embedding matrix for the original scales;
#'   row names are `scale_id`. When `NULL`, it is computed with `embed_fun`.
#' @param embed_fun Function that takes a character vector and returns an
#'   embedding matrix (one row per text), using the same model as `item_emb`.
#' @param n_steps Number of relabeling steps.
#' @param probs Quantiles used for the low and high cutoffs. Default
#'   `c(.25, .75)`.
#' @param provider,model,chat_args LLM settings passed to [llm_label_items()].
#' @param max_items Maximum number of items sent to the LLM per scale. Larger
#'   scales are represented by the items closest to the scale centroid.
#' @param min_words,max_words Suggested length of the definitions.
#' @param definition_template Optional template the LLM follows when it writes
#'   definitions, e.g. [definition_template()]. Use the same template as for
#'   the original definitions when they were harmonized with
#'   [harmonize_definitions()], so all definitions share one format.
#' @param jingle_patience Maximum number of times the same pair of scales is
#'   contrasted. With `1` (default) a pair gets one rewrite; if it is still a
#'   jingle afterwards it is unresolved (see `unresolved` in the output). The
#'   first rewrite usually resolves the jingle; repeated rewrites of the same
#'   pair tend to return the same definitions and loop. Use `2` for a second
#'   try, `Inf` for no limit, and `0` to turn contrasts off: only jangle
#'   pairs are treated (merge-only), and the original definitions stay
#'   untouched except where scales merge. A merged scale is a new scale, so
#'   its pairs start again at zero.
#' @param transform How similarities are reported: `"none"` (raw cosines),
#'   `"rescale"` or `"percentile"` (see [transform_similarity()]). Applied to
#'   the scale-scale, definition-definition and scale-definition matrices of
#'   every step. The jingle/jangle classification and `own_rank` do not change
#'   (cutoffs are quantiles), but the order in which competing pairs are acted
#'   on does: pairs are taken by the gap between item and definition
#'   similarity, and that gap depends on the scale. With `"percentile"` both
#'   similarities are on the same footing, so the gap compares like with like.
#'   `pairs`, `own_cos` and the summary values are on the chosen scale.
#'   ICLUST's alpha and beta always use raw cosines.
#' @param test_col Optional name of a column present in both `items` and
#'   `scales` (e.g. `"test"`) giving the test each item and original scale
#'   comes from. Needed for `center_by_test` and `between_tests_only`.
#' @param center_by_test If `TRUE`, item embeddings are centered by test and
#'   definition embeddings by "author" before computing similarities (see
#'   [center_embeddings()]): each original definition minus the mean of the
#'   original definitions of its test, each LLM definition minus the mean of
#'   all LLM definitions written so far. This removes the style shared by the
#'   texts of one source.
#' @param center_similarities If `TRUE`, the test effect is removed from the
#'   similarities instead of (or in addition to) the embeddings: in every
#'   step the scale-scale, definition-definition and scale-definition matrices
#'   go through [center_similarity()], which subtracts the mean of each block
#'   of groups (e.g. FacetMap x FacetMap) and restores the original mean and
#'   SD. Scales are grouped by test (`"mixed"` for merged scales that combine
#'   tests); definitions by author (their test, or `"llm"`). Unlike
#'   `center_by_test`, the similarities keep their metric.
#' @param between_tests_only If `TRUE`, only pairs that are not within one test
#'   trigger merges or contrasts. A pair is within a test when both scales
#'   contain items of that single test only; merged scales that mix tests are
#'   always between. Cutoffs are still computed from all pairs.
#' @param cache_dir Folder where LLM answers and new definition embeddings are
#'   cached. Use one folder per embedding model.
#' @param verbose Print progress.
#'
#' @return A list of class `empirical_relabeling` with tidy tables:
#' \describe{
#'   \item{`items`}{`item_id`, `item_text`, `scale_id` (original scale).}
#'   \item{`membership`}{`scale_id`, `item_id` for every scale ever created.}
#'   \item{`merges`}{`step`, `scale_id` (new scale), `parent` (merged scale):
#'     the edges of the merge tree.}
#'   \item{`labels`}{`label_id`, `scale_id`, `step`, `source`
#'     (`"original"`, `"merge"`, `"contrast"`), `label`, `definition`: every
#'     label a scale ever had.}
#'   \item{`scales_by_step`}{`step`, `scale_id`, `label_id`, `n_items`,
#'     `own_cos` (similarity of the scale with its own definition),
#'     `own_rank` (rank of its own definition among all definitions).}
#'   \item{`pairs`}{Classified scale pairs of every step, with `contrasts`
#'     (times the pair has been contrasted so far) and `unresolved` (a jingle
#'     that has reached `jingle_patience`).}
#'   \item{`unresolved`}{Jingle pairs of the last step that reached
#'     `jingle_patience`: scales whose definitions stay alike, even after being
#'     rewritten to differ, although their items differ.}
#'   \item{`thresholds`}{Cutoffs of every step.}
#'   \item{`actions`}{One row per LLM call: `step`, `action`, `scale_1`,
#'     `scale_2`, `new_scale`, the pair similarities, and the `prompt`.}
#'   \item{`item_emb`, `label_emb`}{Embedding matrices (rows = `item_id` and
#'     `label_id`). `item_emb` is centered when `center_by_test = TRUE`;
#'     `label_emb` always holds the raw definition embeddings.}
#'   \item{`settings`}{The arguments used.}
#' }
#' @export
empirical_relabeling <- function(
    items,
    scales,
    item_emb,
    def_emb = NULL,
    embed_fun,
    n_steps = 5,
    probs = c(.25, .75),
    provider = "openai",
    model = "gpt-4.1-mini",
    chat_args = list(),
    max_items = 60,
    min_words = 12,
    max_words = 60,
    definition_template = NULL,
    jingle_patience = 1,
    transform = c("none", "rescale", "percentile"),
    test_col = NULL,
    center_by_test = FALSE,
    center_similarities = FALSE,
    between_tests_only = FALSE,
    cache_dir = "relabel_cache",
    verbose = TRUE) {

  if (!all(c("item_id", "item_text", "scale_id") %in% names(items))) {
    stop("`items` must have columns item_id, item_text and scale_id.", call. = FALSE)
  }
  if (!all(c("scale_id", "label", "definition") %in% names(scales))) {
    stop("`scales` must have columns scale_id, label and definition.", call. = FALSE)
  }
  if (!setequal(unique(items$scale_id), scales$scale_id)) {
    stop("Every item scale must have a definition in `scales`, and vice versa.",
         call. = FALSE)
  }
  transform <- match.arg(transform)
  if (!is.numeric(jingle_patience) || length(jingle_patience) != 1 || jingle_patience < 0) {
    stop("`jingle_patience` must be a number >= 0 (0 = no contrasts, Inf = no limit).",
         call. = FALSE)
  }
  if ((center_by_test || center_similarities || between_tests_only) &&
      (is.null(test_col) || !test_col %in% names(items) || !test_col %in% names(scales))) {
    stop("`center_by_test`, `center_similarities` and `between_tests_only` need `test_col`, a column ",
         "present in both `items` and `scales`.", call. = FALSE)
  }
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  item_test <- if (is.null(test_col)) NA_character_ else as.character(items[[test_col]])
  items <- dplyr::tibble(item_id = as.character(items$item_id),
                         item_text = items$item_text,
                         scale_id = as.character(items$scale_id),
                         test = item_test)
  item_text <- stats::setNames(items$item_text, items$item_id)
  item_emb <- as.matrix(item_emb)[items$item_id, , drop = FALSE]
  if (center_by_test) item_emb <- center_embeddings(item_emb, items$test)

  if (is.null(def_emb)) {
    def_emb <- embeddcv_cached_embed(scales$definition, embed_fun, cache_dir)
    rownames(def_emb) <- scales$scale_id
  }
  def_emb <- as.matrix(def_emb)[scales$scale_id, , drop = FALSE]

  labels <- dplyr::tibble(
    label_id = paste0(scales$scale_id, "@0"),
    scale_id = scales$scale_id,
    step = 0L,
    source = "original",
    label = scales$label,
    definition = scales$definition,
    group = if (is.null(test_col)) "original" else as.character(scales[[test_col]])
  )
  label_emb <- def_emb
  rownames(label_emb) <- labels$label_id

  membership <- dplyr::select(items, "scale_id", "item_id")
  merges <- dplyr::tibble(step = integer(), scale_id = character(), parent = character())
  actions <- list()
  current <- dplyr::tibble(scale_id = scales$scale_id, label_id = labels$label_id)
  by_step <- list()
  # times each pair has been contrasted; key = the two scale ids, sorted
  pair_key <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = " | ")
  contrasts_done <- integer()
  n_contrasts <- function(a, b) {
    n <- unname(contrasts_done[pair_key(a, b)])
    ifelse(is.na(n), 0L, n)
  }

  llm <- function(sets, current_labels = NULL) {
    llm_label_items(sets, current_labels = current_labels, provider = provider,
                    model = model, chat_args = chat_args, min_words = min_words,
                    max_words = max_words, template = definition_template,
                    cache_dir = cache_dir)
  }
  # items sent to the LLM: the max_items items closest to the scale centroid
  item_set <- function(ids) {
    if (length(ids) > max_items) {
      emb <- item_emb[ids, , drop = FALSE]
      sim <- cosine_matrix(rbind(colMeans(emb), emb))[1, -1]
      ids <- ids[sort(order(sim, decreasing = TRUE)[seq_len(max_items)])]
    }
    unname(item_text[ids])
  }
  label_of <- stats::setNames(labels$label, labels$label_id)
  def_of <- stats::setNames(labels$definition, labels$label_id)

  for (step in 0:n_steps) {
    if (step > 0) {
      prev <- by_step[[step]]$pairs
      if (between_tests_only) prev <- prev[prev$between, ]
      jangle <- embeddcv_disjoint_pairs(prev[prev$type == "Jangle fallacy", ])
      used <- c(jangle$scale_1, jangle$scale_2)
      # jingle pairs contrasted `jingle_patience` times already are unresolved:
      # they are not sent back to the LLM
      jingle <- prev[prev$type == "Jingle fallacy" & prev$contrasts < jingle_patience &
                       !prev$scale_1 %in% used & !prev$scale_2 %in% used, ]
      jingle <- embeddcv_disjoint_pairs(jingle)

      if (nrow(jangle) + nrow(jingle) == 0) {
        n_left <- sum(prev$type == "Jingle fallacy")
        if (verbose) {
          message("Step ", step, ": no fallacies left to act on",
                  if (n_left > 0) paste0(" (", n_left, " unresolved jingle pairs)"),
                  ". Stopping.")
        }
        break
      }
      if (verbose) {
        message("Step ", step, ": merging ", nrow(jangle), " jangle pairs, relabeling ",
                nrow(jingle), " jingle pairs (", nrow(current), " scales).")
      }

      new_labels <- list()
      step_actions <- list()

      # Jangle: merge the two scales and label the combined items.
      for (k in seq_len(nrow(jangle))) {
        parents <- c(jangle$scale_1[k], jangle$scale_2[k])
        new_id <- paste0("S", step, ".", k)
        ids <- membership$item_id[membership$scale_id %in% parents]
        membership <- dplyr::bind_rows(membership, dplyr::tibble(scale_id = new_id, item_id = ids))
        merges <- dplyr::bind_rows(merges, dplyr::tibble(step = step, scale_id = new_id, parent = parents))
        res <- llm(list(item_set(ids)))
        new_labels[[length(new_labels) + 1]] <- dplyr::tibble(
          scale_id = new_id, source = "merge", label = res$label, definition = res$definition)
        step_actions[[length(step_actions) + 1]] <- dplyr::tibble(
          action = "merge", scale_1 = parents[1], scale_2 = parents[2], new_scale = new_id,
          scale_cos = jangle$scale_cos[k], label_cos = jangle$label_cos[k],
          prompt = attr(res, "prompt"))
      }

      # Jingle: relabel both scales so they fit their items and differ.
      lab_now <- stats::setNames(current$label_id, current$scale_id)
      for (k in seq_len(nrow(jingle))) {
        pair <- c(jingle$scale_1[k], jingle$scale_2[k])
        sets <- lapply(pair, function(s) item_set(membership$item_id[membership$scale_id == s]))
        cur <- paste0(label_of[lab_now[pair]], " (", def_of[lab_now[pair]], ")")
        res <- llm(sets, current_labels = cur)
        new_labels[[length(new_labels) + 1]] <- dplyr::tibble(
          scale_id = pair, source = "contrast", label = res$label, definition = res$definition)
        step_actions[[length(step_actions) + 1]] <- dplyr::tibble(
          action = "contrast", scale_1 = pair[1], scale_2 = pair[2], new_scale = NA_character_,
          scale_cos = jingle$scale_cos[k], label_cos = jingle$label_cos[k],
          prompt = attr(res, "prompt"))
      }

      for (k in seq_len(nrow(jingle))) {
        key <- pair_key(jingle$scale_1[k], jingle$scale_2[k])
        contrasts_done[key] <- n_contrasts(jingle$scale_1[k], jingle$scale_2[k]) + 1L
      }

      new_labels <- dplyr::bind_rows(new_labels)
      new_labels <- dplyr::mutate(new_labels, label_id = paste0(.data$scale_id, "@", step),
                                  step = step, .before = 1)
      new_labels$group <- "llm"
      emb <- embeddcv_cached_embed(new_labels$definition, embed_fun, cache_dir)
      rownames(emb) <- new_labels$label_id
      label_emb <- rbind(label_emb, emb[, colnames(label_emb), drop = FALSE])
      labels <- dplyr::bind_rows(labels, new_labels)
      label_of <- stats::setNames(labels$label, labels$label_id)
      def_of <- stats::setNames(labels$definition, labels$label_id)
      actions[[step]] <- dplyr::mutate(dplyr::bind_rows(step_actions), step = step, .before = 1)

      # New active set: drop merged scales, add the new ones, update labels.
      current <- current[!current$scale_id %in% used, ]
      relabeled <- new_labels$source == "contrast"
      current$label_id[match(new_labels$scale_id[relabeled], current$scale_id)] <-
        new_labels$label_id[relabeled]
      current <- dplyr::bind_rows(
        current,
        dplyr::select(new_labels[new_labels$source == "merge", ], "scale_id", "label_id")
      )
    }

    cl <- embeddcv_classify_step(
      current, membership, item_emb, label_emb, label_of, probs,
      transform = transform,
      label_group = if (!is.null(test_col)) stats::setNames(labels$group, labels$label_id),
      item_test = if (!is.null(test_col)) stats::setNames(items$test, items$item_id),
      center_definitions = center_by_test,
      center_similarities = center_similarities
    )
    cl$pairs$contrasts <- n_contrasts(cl$pairs$scale_1, cl$pairs$scale_2)
    cl$pairs$unresolved <- cl$pairs$type == "Jingle fallacy" &
      cl$pairs$contrasts >= jingle_patience
    by_step[[step + 1]] <- cl
    if (verbose) {
      message("Step ", step, ": ", nrow(current), " scales, ",
              sum(cl$pairs$type == "Jingle fallacy"), " jingle, ",
              sum(cl$pairs$type == "Jangle fallacy"), " jangle.")
    }
  }

  steps <- seq_along(by_step) - 1L
  bind_steps <- function(name) {
    dplyr::bind_rows(lapply(seq_along(by_step), function(i) {
      dplyr::mutate(dplyr::as_tibble(by_step[[i]][[name]]), step = steps[i], .before = 1)
    }))
  }

  structure(
    list(
      items = items,
      membership = membership,
      merges = merges,
      labels = labels,
      scales_by_step = bind_steps("scales"),
      pairs = bind_steps("pairs"),
      thresholds = bind_steps("thresholds"),
      actions = dplyr::bind_rows(actions),
      unresolved = dplyr::as_tibble(
        by_step[[length(by_step)]]$pairs[by_step[[length(by_step)]]$pairs$unresolved, ]
      ),
      item_emb = item_emb,
      label_emb = label_emb,
      settings = list(n_steps = n_steps, probs = probs, provider = provider,
                      model = model, max_items = max_items, test_col = test_col,
                      definition_template = definition_template,
                      jingle_patience = jingle_patience,
                      transform = transform,
                      center_by_test = center_by_test,
                      center_similarities = center_similarities,
                      between_tests_only = between_tests_only)
    ),
    class = c("empirical_relabeling", "list")
  )
}

# Classify the scale pairs of one step and compute how well each scale
# matches its own definition. `label_group` (label_id -> author group) and
# `item_test` (item_id -> test) give the groups used to center the definition
# embeddings (`center_definitions`) or the similarities
# (`center_similarities`); `item_test` also adds the `between` column.
embeddcv_classify_step <- function(current, membership, item_emb, label_emb, label_of, probs,
                                   transform = "none", label_group = NULL, item_test = NULL,
                                   center_definitions = FALSE, center_similarities = FALSE) {
  mem <- membership[membership$scale_id %in% current$scale_id, ]
  scale_emb <- get_scale_embeddings(item_emb[mem$item_id, , drop = FALSE], mem$scale_id)
  scale_emb <- scale_emb[current$scale_id, , drop = FALSE]
  def_emb <- label_emb[current$label_id, , drop = FALSE]
  if (!is.null(item_test)) {
    # tests of each scale; "mixed" for merged scales combining tests
    tests <- tapply(item_test[mem$item_id], mem$scale_id, function(t) sort(unique(t)))
    one_test <- vapply(tests, function(t) if (length(t) == 1) t else NA_character_, "")
    scale_group <- ifelse(is.na(one_test), "mixed", one_test)[current$scale_id]
  }
  if (center_definitions) {
    # subtract the mean of all definitions written by the same author group
    # (groups of one are left as they are)
    grp <- label_group[rownames(label_emb)]
    sums <- rowsum(label_emb, grp)
    n_grp <- as.vector(table(grp)[rownames(sums)])
    means <- sums / n_grp
    means[n_grp < 2, ] <- 0
    def_emb <- def_emb - means[label_group[current$label_id], , drop = FALSE]
  }
  rownames(def_emb) <- current$scale_id

  n <- nrow(current)
  scale_cos <- cosine_matrix(scale_emb)
  def_cos <- cosine_matrix(def_emb)
  cross <- cosine_matrix(rbind(scale_emb, def_emb))[seq_len(n), n + seq_len(n), drop = FALSE]
  if (center_similarities) {
    def_group <- label_group[current$label_id]
    scale_cos <- center_similarity(scale_cos, scale_group)
    def_cos <- center_similarity(def_cos, def_group)
    cross <- center_similarity(cross, scale_group, def_group)
  }
  scale_cos <- transform_similarity(scale_cos, transform)
  def_cos <- transform_similarity(def_cos, transform)
  cross <- transform_similarity(cross, transform)

  pairs <- jingle_jangle_pairs(scale_cos, def_cos,
                               labels = unname(label_of[current$label_id]))
  thresholds <- jingle_jangle_thresholds(pairs$scale_cos, pairs$label_cos, probs = probs)
  pairs <- classify_jingle_jangle(pairs, thresholds, verbose = FALSE)
  pairs <- pairs[, c("scale_1", "scale_2", "label_1", "label_2",
                     "scale_cos", "label_cos", "type")]

  if (!is.null(item_test)) {
    # a pair is within a test when both scales are pure and share that test
    pairs$test_1 <- vapply(tests[pairs$scale_1], paste, "", collapse = "+")
    pairs$test_2 <- vapply(tests[pairs$scale_2], paste, "", collapse = "+")
    t1 <- one_test[pairs$scale_1]
    pairs$between <- is.na(t1) | is.na(one_test[pairs$scale_2]) | t1 != one_test[pairs$scale_2]
  }

  list(
    pairs = pairs,
    thresholds = as.list(thresholds),
    scales = dplyr::tibble(
      scale_id = current$scale_id,
      label_id = current$label_id,
      n_items = as.integer(table(mem$scale_id)[current$scale_id]),
      own_cos = diag(cross),
      own_rank = vapply(seq_len(n), function(i) as.integer(rank(-cross[i, ])[i]), integer(1))
    )
  )
}

# Keep pairs with no scale in common, preferring the largest gap between
# scale and label similarity.
embeddcv_disjoint_pairs <- function(pairs) {
  pairs <- pairs[order(-abs(pairs$scale_cos - pairs$label_cos)), ]
  used <- character()
  keep <- logical(nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    if (!pairs$scale_1[i] %in% used && !pairs$scale_2[i] %in% used) {
      keep[i] <- TRUE
      used <- c(used, pairs$scale_1[i], pairs$scale_2[i])
    }
  }
  pairs[keep, ]
}

embeddcv_cached_embed <- function(text, embed_fun, cache_dir) {
  path <- file.path(cache_dir, paste0("emb_", rlang::hash(text), ".rds"))
  if (file.exists(path)) return(readRDS(path))
  emb <- as.matrix(embed_fun(unname(as.character(text))))
  saveRDS(emb, path)
  emb
}

#' Summarize an empirical relabeling run by step
#'
#' @param x Output of [empirical_relabeling()].
#'
#' @return Tibble with one row per step: number of scales, merges and
#'   contrasts that produced the step, jingle and jangle counts (how many
#'   jingles are unresolved, i.e. reached `jingle_patience`, and how many
#'   fallacies are between tests, when `test_col` was given), cutoffs,
#'   the correlation between scale and label similarities across pairs
#'   (`r_scale_label`), the mean similarity of each scale with its own
#'   definition (`mean_own_cos`), and the share of scales whose own definition
#'   is their best-matching definition (`prop_own_best`).
#' @export
relabeling_summary <- function(x) {
  fit <- dplyr::summarise(
    dplyr::group_by(x$scales_by_step, .data$step),
    n_scales = dplyr::n(),
    mean_own_cos = mean(.data$own_cos),
    prop_own_best = mean(.data$own_rank == 1)
  )
  pairs <- x$pairs
  if (is.null(pairs$between)) pairs$between <- NA
  if (is.null(pairs$unresolved)) pairs$unresolved <- NA
  fallacies <- dplyr::summarise(
    dplyr::group_by(pairs, .data$step),
    jingle = sum(.data$type == "Jingle fallacy"),
    jingle_unresolved = sum(.data$unresolved),
    jangle = sum(.data$type == "Jangle fallacy"),
    jingle_between = sum(.data$type == "Jingle fallacy" & .data$between),
    jangle_between = sum(.data$type == "Jangle fallacy" & .data$between),
    r_scale_label = stats::cor(.data$scale_cos, .data$label_cos)
  )
  acts <- dplyr::summarise(
    dplyr::group_by(x$actions, .data$step),
    merges = sum(.data$action == "merge"),
    contrasts = sum(.data$action == "contrast")
  )
  out <- dplyr::left_join(fit, acts, by = "step")
  out <- dplyr::left_join(out, fallacies, by = "step")
  out <- dplyr::left_join(out, x$thresholds, by = "step")
  out$merges[is.na(out$merges)] <- 0L
  out$contrasts[is.na(out$contrasts)] <- 0L
  dplyr::relocate(out, "merges", "contrasts", .after = "n_scales")
}

#' Active scales at one step of an empirical relabeling run
#'
#' @param x Output of [empirical_relabeling()].
#' @param step Step to show. Defaults to the last step.
#'
#' @return Tibble with one row per active scale: its current label and
#'   definition, where the label came from, number of items, fit with its own
#'   definition, and the original scales it contains.
#' @export
relabeling_scales <- function(x, step = max(x$scales_by_step$step)) {
  s <- step
  active <- x$scales_by_step[x$scales_by_step$step == s, ]
  active <- dplyr::left_join(
    active,
    dplyr::select(x$labels, "label_id", "label", "definition", "source", label_step = "step"),
    by = "label_id"
  )
  orig_label <- stats::setNames(x$labels$label[x$labels$step == 0],
                                x$labels$scale_id[x$labels$step == 0])
  originals <- dplyr::left_join(
    x$membership[x$membership$scale_id %in% active$scale_id, ],
    dplyr::select(x$items, "item_id", original = "scale_id"),
    by = "item_id"
  )
  originals <- dplyr::summarise(
    dplyr::group_by(dplyr::distinct(originals, .data$scale_id, .data$original), .data$scale_id),
    n_original = dplyr::n(),
    original_scales = paste(sort(orig_label[.data$original]), collapse = "; ")
  )
  out <- dplyr::left_join(active, originals, by = "scale_id")
  dplyr::arrange(out, dplyr::desc(.data$n_original), dplyr::desc(.data$n_items))
}

# Layout shared by the tree plots: the merge tree up to `steps`, the leaf
# order (original scales, grouped by the cluster they end in) and a position
# for every node (leaves 1..n, a merged scale at the mean of its parts).
embeddcv_merge_layout <- function(x, steps, drop_unmerged = FALSE) {
  steps <- min(steps, max(x$scales_by_step$step))
  merges <- x$merges[x$merges$step <= steps, ]
  children <- split(merges$parent, merges$scale_id)
  roots <- x$scales_by_step$scale_id[x$scales_by_step$step == steps]
  if (drop_unmerged) roots <- intersect(roots, merges$scale_id)

  leaves_of <- function(id) {
    if (is.null(children[[id]])) id else unlist(lapply(children[[id]], leaves_of))
  }
  family <- function(id) c(id, unlist(lapply(children[[id]], family)))
  leaves <- unlist(lapply(roots, leaves_of))
  pos <- stats::setNames(seq_along(leaves), leaves)
  for (id in unique(merges$scale_id)) {
    if (all(children[[id]] %in% names(pos))) pos[id] <- mean(pos[children[[id]]])
  }
  root_of <- unlist(lapply(roots, function(r) {
    ids <- family(r)
    stats::setNames(rep(r, length(ids)), ids)
  }))

  list(steps = steps, merges = merges, children = children, roots = roots,
       merged_roots = intersect(roots, merges$scale_id), leaves = leaves,
       pos = pos, root_of = root_of,
       born = tapply(x$labels$step, x$labels$scale_id, min))
}

# Current label of each scale at `steps` (its most recent label so far).
embeddcv_label_at <- function(x, ids, steps) {
  l <- x$labels[x$labels$scale_id %in% ids & x$labels$step <= steps, ]
  l <- l[order(l$scale_id, -l$step), ]
  l <- l[!duplicated(l$scale_id), ]
  stats::setNames(l$label, l$scale_id)[ids]
}

# Colour per node: one palette colour per merged cluster at `steps`, grey for
# scales that were never merged.
embeddcv_cluster_colours <- function(lay, palette) {
  cols <- stats::setNames(
    embeddcv_relabel_palette(length(lay$merged_roots), palette, begin = .1, end = .85),
    lay$merged_roots)
  out <- stats::setNames(rep("grey60", length(lay$pos)), names(lay$pos))
  in_cluster <- names(lay$root_of)[lay$root_of %in% lay$merged_roots]
  out[in_cluster] <- cols[lay$root_of[in_cluster]]
  out
}

#' Plot the merge tree of an empirical relabeling run
#'
#' Interactive dendrogram (plotly). The original scales are on the left (y
#' axis); each merge is a node at the step where it happened. Diamonds mark
#' jingle relabelings of a scale. Each node shows the short label; hovering
#' shows the definition.
#'
#' @param x Output of [empirical_relabeling()].
#' @param steps Last step to draw.
#' @param show_text Which label sources to print next to the nodes (`"original"`,
#'   `"merge"`, `"contrast"`). The others show their label on hover only.
#' @param wrap Width used to wrap the definitions in the hover box.
#' @param show_items Show the items of each original scale (step 0) in its
#'   hover box, below the definition.
#' @param max_items Maximum number of items listed per scale; longer lists end
#'   with "... and N more".
#' @param height Plot height in pixels. Defaults to 16 px per original scale.
#'
#' @return A plotly object.
#' @export
plot_relabeling_tree <- function(x, steps = max(x$scales_by_step$step),
                                 show_text = "merge", wrap = 60, show_items = TRUE,
                                 max_items = 15, height = NULL) {
  lay <- embeddcv_merge_layout(x, steps)
  y <- lay$pos
  end <- stats::setNames(rep(lay$steps, length(y)), names(y))
  end[lay$merges$parent] <- lay$merges$step

  segs <- rbind(
    data.frame(x = lay$born[names(y)], xend = end[names(y)], y = y, yend = y),
    do.call(rbind, lapply(names(lay$children), function(id) {
      data.frame(x = lay$born[[id]], xend = lay$born[[id]],
                 y = min(y[lay$children[[id]]]), yend = max(y[lay$children[[id]]]))
    }))
  )

  nodes <- x$labels[x$labels$step <= lay$steps, ]
  nodes$y <- y[nodes$scale_id]
  n_items <- table(x$membership$scale_id)
  nodes$hover <- paste0(
    "<b>", nodes$label, "</b><br>",
    "scale ", nodes$scale_id, " | ", n_items[nodes$scale_id], " items | step ",
    nodes$step, " (", nodes$source, ")<br><br>",
    gsub("\n", "<br>", stringr::str_wrap(nodes$definition, wrap))
  )
  if (show_items) {
    # bulleted item list for the original scales (step 0)
    item_list <- vapply(split(x$items$item_text, x$items$scale_id), function(it) {
      more <- length(it) - max_items
      it <- utils::head(it, max_items)
      it <- gsub("\n", "<br>&nbsp;&nbsp;", stringr::str_wrap(it, wrap - 2))
      paste0("<br><br><i>Items:</i><br>", paste0("\u2022 ", it, collapse = "<br>"),
             if (more > 0) paste0("<br>... and ", more, " more"))
    }, character(1))
    orig <- nodes$source == "original"
    nodes$hover[orig] <- paste0(nodes$hover[orig], item_list[nodes$scale_id[orig]])
  }

  cols <- c(original = "#5F6B73", merge = "#B23A7A", contrast = "#2C7FB8")
  symbols <- c(original = "circle", merge = "circle", contrast = "diamond")
  leaf_labels <- embeddcv_label_at(x, lay$leaves, 0)

  p <- plotly::plot_ly(height = height %||% (16 * length(lay$leaves) + 150))
  p <- plotly::add_segments(p, data = segs, x = ~x, xend = ~xend, y = ~y, yend = ~yend,
                            line = list(color = "#B8C0C6", width = 1),
                            hoverinfo = "none", showlegend = FALSE)
  for (src in intersect(names(cols), unique(nodes$source))) {
    d <- nodes[nodes$source == src, ]
    d$text <- if (src %in% show_text) d$label else ""
    p <- plotly::add_trace(
      p, data = d, x = ~step, y = ~y, type = "scatter", mode = "markers+text",
      text = ~text, textposition = "top right",
      textfont = list(size = 10, color = cols[[src]]),
      hovertext = ~hover, hoverinfo = "text", name = src,
      marker = list(color = cols[[src]], size = 7, symbol = symbols[[src]])
    )
  }
  plotly::layout(
    p,
    xaxis = list(title = "Step", dtick = 1, range = c(-.3, lay$steps + .8), zeroline = FALSE),
    yaxis = list(title = "", tickvals = seq_along(lay$leaves), ticktext = unname(leaf_labels),
                 tickfont = list(size = 9), automargin = TRUE, zeroline = FALSE,
                 showgrid = FALSE),
    legend = list(orientation = "h", y = 1.02, yanchor = "bottom"),
    hoverlabel = list(align = "left")
  )
}

#' Plot how an empirical relabeling run changes over steps
#'
#' Four panels: number of scales, jingle and jangle counts, cutoffs, and the
#' fit between scale content and labels.
#'
#' @param x Output of [empirical_relabeling()].
#'
#' @return A ggplot object.
#' @export
plot_relabeling_trajectory <- function(x) {
  s <- relabeling_summary(x)
  panels <- c(
    n_scales = "Number of scales",
    jingle = "Fallacies", jangle = "Fallacies",
    scale_low = "Cutoffs", scale_high = "Cutoffs",
    label_low = "Cutoffs", label_high = "Cutoffs",
    mean_own_cos = "Label-content fit", prop_own_best = "Label-content fit",
    r_scale_label = "Label-content fit"
  )
  long <- tidyr::pivot_longer(s, dplyr::all_of(names(panels)),
                              names_to = "metric", values_to = "value")
  long$panel <- factor(panels[long$metric], levels = unique(panels))

  ggplot2::ggplot(long, ggplot2::aes(.data$step, .data$value, colour = .data$metric)) +
    ggplot2::geom_line(linewidth = .8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~ panel, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = s$step) +
    ggplot2::labs(x = "Step", y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

#' Circular merge tree of an empirical relabeling run
#'
#' Static radial dendrogram in the style of Wulff & Mata (2025, Fig. 4C). The
#' original scales are around the rim; each merge is an arc placed further
#' inside the circle the later it happened. Branches are coloured by the
#' cluster they belong to at step `steps`, and those clusters are labelled
#' inside the circle with their current short label.
#'
#' @param x Output of [empirical_relabeling()].
#' @param steps Last step to draw. Merges after it are ignored, so smaller
#'   values show earlier, finer solutions.
#' @param label_merges Which merged scales to label: `"final"` (clusters at
#'   step `steps`), `"all"` (every merge node) or `"none"`.
#' @param rim_size,label_size Text size of the original scale labels and of
#'   the cluster labels.
#' @param wrap Width used to wrap the cluster labels.
#' @param palette Palette for the clusters (see [plot_relabeling_figure()]).
#'
#' @return A ggplot object.
#' @export
plot_relabeling_circle <- function(x, steps = max(x$scales_by_step$step),
                                   label_merges = c("final", "all", "none"),
                                   rim_size = 1.9, label_size = 2.4, wrap = 18,
                                   palette = "magma") {
  label_merges <- match.arg(label_merges)
  lay <- embeddcv_merge_layout(x, steps)
  theta <- 2 * pi * (lay$pos - .5) / length(lay$leaves)
  radius <- function(step) 1 - min(.8 / max(lay$steps, 1), .12) * step
  col_of <- embeddcv_cluster_colours(lay, palette)
  merged <- names(lay$children)
  born <- lay$born

  spokes <- data.frame(
    theta = theta[lay$merges$parent], col = col_of[lay$merges$parent],
    r0 = radius(born[lay$merges$parent]), r1 = radius(lay$merges$step)
  )
  arcs <- do.call(rbind, lapply(merged, function(id) {
    a <- seq(min(theta[lay$children[[id]]]), max(theta[lay$children[[id]]]), length.out = 40)
    data.frame(id = id, theta = a, r = radius(born[[id]]), col = col_of[[id]])
  }))
  nodes <- data.frame(theta = theta[merged], r = radius(born[merged]), col = col_of[merged])

  # text angle that keeps labels readable on the left half of the circle
  text_angle <- function(th) {
    deg <- th * 180 / pi
    flip <- deg > 90 & deg < 270
    list(angle = ifelse(flip, deg + 180, deg), flip = flip)
  }

  leaves <- lay$leaves
  ta <- text_angle(theta[leaves])
  rim <- data.frame(theta = theta[leaves], label = embeddcv_label_at(x, leaves, 0),
                    angle = ta$angle, hjust = ifelse(ta$flip, 1, 0), col = col_of[leaves])

  lab_ids <- switch(label_merges, final = lay$merged_roots, all = merged, none = character())
  ta <- text_angle(theta[lab_ids])
  r_lab <- radius(born[lab_ids]) - .02
  inner <- data.frame(
    x = r_lab * cos(theta[lab_ids]), y = r_lab * sin(theta[lab_ids]),
    label = stringr::str_wrap(embeddcv_label_at(x, lab_ids, lay$steps), wrap),
    angle = ta$angle, hjust = ifelse(ta$flip, 0, 1),
    col = mix_col(col_of[lab_ids], "black", .25)
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = spokes,
      ggplot2::aes(x = .data$r0 * cos(.data$theta), y = .data$r0 * sin(.data$theta),
                   xend = .data$r1 * cos(.data$theta), yend = .data$r1 * sin(.data$theta),
                   colour = .data$col), linewidth = .45) +
    ggplot2::geom_path(
      data = arcs,
      ggplot2::aes(x = .data$r * cos(.data$theta), y = .data$r * sin(.data$theta),
                   group = .data$id, colour = .data$col), linewidth = .45) +
    ggplot2::geom_point(
      data = rim, ggplot2::aes(x = cos(.data$theta), y = sin(.data$theta), colour = .data$col),
      size = .8) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$r * cos(.data$theta), y = .data$r * sin(.data$theta),
                   colour = .data$col), size = 1.2) +
    ggplot2::geom_text(
      data = rim,
      ggplot2::aes(x = 1.03 * cos(.data$theta), y = 1.03 * sin(.data$theta),
                   label = .data$label, angle = .data$angle, hjust = .data$hjust,
                   colour = .data$col), size = rim_size) +
    ggplot2::geom_text(
      data = inner,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, angle = .data$angle,
                   hjust = .data$hjust, colour = .data$col),
      size = label_size, lineheight = .8, fontface = "bold") +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
    ggplot2::labs(title = paste0("Step ", lay$steps, ": ", length(lay$roots), " scales (",
                                 length(lay$merged_roots), " merged)")) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))
}

#' ICLUST-style diagram of an empirical relabeling run
#'
#' Static cluster diagram in the style of `psych::ICLUST.diagram()`. Original
#' scales are boxes on the left; each merged scale is an ellipse placed at the
#' step where it was formed, showing its short label, its semantic alpha
#' (\eqn{\alpha}), its semantic beta (\eqn{\beta}) and its number of items.
#' Arrows go from each merged scale to the two scales it combined, labelled
#' with the cosine similarity between the two scale embeddings (the analogue
#' of ICLUST's loadings).
#'
#' @details
#' Indices are computed from the item-item cosine matrix \eqn{C}:
#' \describe{
#'   \item{\eqn{\alpha}}{\eqn{k\bar{c}/(1+(k-1)\bar{c})}, with \eqn{\bar{c}} the
#'     mean item-item cosine of the scale (as in [semantic_validity()]).}
#'   \item{\eqn{\beta}}{split-half reliability between the two merged parts
#'     \eqn{A} and \eqn{B}: \eqn{4\sum C_{AB} / \sum C_{A \cup B}}, i.e.
#'     ICLUST's beta for the split at which the scale was formed.}
#' }
#' Reverse-keyed items lower both indices, because their embeddings point
#' away from the other items.
#'
#' @param x Output of [empirical_relabeling()].
#' @param steps Last step to draw.
#' @param drop_unmerged Leave out original scales that were never merged up to
#'   `steps`.
#' @param box_size,ellipse_size,edge_size Text sizes of the boxes, ellipses and
#'   arrow labels.
#' @param wrap Width used to wrap the ellipse labels.
#' @param label_space Room on the left for the boxes, in step units.
#' @param palette Palette for the clusters (see [plot_relabeling_figure()]).
#'
#' @return A ggplot object. Use a tall device: about 0.25 in per box.
#' @export
plot_relabeling_iclust <- function(x, steps = max(x$scales_by_step$step),
                                   drop_unmerged = TRUE, box_size = 2.2,
                                   ellipse_size = 1.9, edge_size = 1.8, wrap = 20,
                                   label_space = 2.5, palette = "magma") {
  lay <- embeddcv_merge_layout(x, steps, drop_unmerged = drop_unmerged)
  merged <- names(lay$children)
  merged <- merged[merged %in% names(lay$pos)]
  y <- -lay$pos
  px <- lay$born[names(y)]
  px[lay$leaves] <- 0
  col_of <- embeddcv_cluster_colours(lay, palette)

  # semantic alpha and beta of each merged scale
  items_of <- split(x$membership$item_id, x$membership$scale_id)
  item_cos <- cosine_matrix(x$item_emb[unique(unlist(items_of[merged])), , drop = FALSE])
  stats_of <- function(id) {
    ab <- lapply(lay$children[[id]], function(s) items_of[[s]])
    all <- unlist(ab)
    k <- length(all)
    cbar <- (sum(item_cos[all, all]) - k) / (k * (k - 1))
    c(alpha = k * cbar / (1 + (k - 1) * cbar),
      beta = 4 * sum(item_cos[ab[[1]], ab[[2]]]) / sum(item_cos[all, all]),
      n = k)
  }
  st <- vapply(merged, stats_of, numeric(3))

  ell <- data.frame(
    id = merged, x = px[merged], y = y[merged], col = col_of[merged],
    label = paste0(
      stringr::str_wrap(embeddcv_label_at(x, merged, lay$steps), wrap), "\n",
      "\u03b1=", sub("^0", "", sprintf("%.2f", st["alpha", ])),
      " \u03b2=", sub("^0", "", sprintf("%.2f", st["beta", ])), " N=", st["n", ])
  )
  a <- .46
  b <- .95
  t <- seq(0, 2 * pi, length.out = 60)
  ell_poly <- do.call(rbind, lapply(seq_len(nrow(ell)), function(i) {
    data.frame(id = ell$id[i], x = ell$x[i] + a * cos(t), y = ell$y[i] + b * sin(t),
               col = ell$col[i])
  }))

  # arrows from each merged scale to its two parts, ending at the box or
  # ellipse border; labels = cosine between the scale embeddings
  scale_emb <- get_scale_embeddings(x$item_emb[x$membership$item_id, , drop = FALSE],
                                    x$membership$scale_id)
  scale_cos <- cosine_matrix(scale_emb[names(y), , drop = FALSE])
  border <- function(id, tx, ty) {
    if (id %in% lay$leaves) return(c(0, y[[id]]))
    ang <- atan2((ty - y[[id]]) / b, (tx - px[[id]]) / a)
    c(px[[id]] + a * cos(ang), y[[id]] + b * sin(ang))
  }
  edges <- do.call(rbind, lapply(merged, function(id) {
    do.call(rbind, lapply(lay$children[[id]], function(ch) {
      from <- border(id, px[[ch]], y[[ch]])
      to <- border(ch, px[[id]], y[[id]])
      data.frame(x = from[1], y = from[2], xend = to[1], yend = to[2],
                 cos = scale_cos[id, ch], col = col_of[[id]])
    }))
  }))

  boxes <- data.frame(x = -.05, y = y[lay$leaves], col = col_of[lay$leaves],
                      label = embeddcv_label_at(x, lay$leaves, 0))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
                   colour = .data$col),
      linewidth = .35, arrow = grid::arrow(length = grid::unit(.06, "in"), type = "closed")) +
    ggplot2::geom_text(
      data = edges,
      ggplot2::aes(x = (.data$x + .data$xend) / 2, y = (.data$y + .data$yend) / 2,
                   label = sub("^0", "", sprintf("%.2f", .data$cos))),
      size = edge_size, colour = "grey30", vjust = -.3) +
    ggplot2::geom_polygon(
      data = ell_poly,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$col),
      fill = "white", linewidth = .4) +
    ggplot2::geom_text(
      data = ell, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = ellipse_size, lineheight = .85) +
    ggplot2::geom_label(
      data = boxes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, colour = .data$col),
      hjust = 1, size = box_size, label.r = grid::unit(0, "pt"),
      label.padding = grid::unit(1.5, "pt")) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = seq_len(lay$steps),
                                limits = c(-label_space, lay$steps + .6)) +
    ggplot2::labs(x = "Step", y = NULL,
                  title = paste0("Merged scales up to step ", lay$steps)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = .5))
}
