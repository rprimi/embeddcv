#' Compute cosine similarities between two embedding sets
#'
#' Computes cosine similarities for any pair of embedding matrices, such as
#' item-item, item-scale, item-definition, scale-scale, scale-definition, or
#' definition-definition. The function keeps the original item-scale interface
#' for backwards compatibility, but now also returns a long pair table and the
#' raw cosine matrix for downstream semantic taxonomic analyses.
#'
#' @param item_emb Numeric matrix or data frame. This is the row embedding set
#'   \eqn{X}: one row per unit to be compared (for example items, scales, or
#'   definitions) and one column per embedding dimension. All columns must be
#'   numeric. Row names are used as default row ids when `x_id = NULL`.
#' @param scale_emb Optional numeric matrix or data frame. This is the column
#'   embedding set \eqn{Y}: one row per comparison target and the same number of
#'   embedding dimensions as `item_emb`. If `NULL`, `item_emb` is compared with
#'   itself, producing a symmetric within-set similarity matrix.
#' @param item_text Optional character vector of length `nrow(item_emb)`.
#'   Legacy row labels/texts used by older item-scale workflows. For new code,
#'   prefer `x_labels`.
#' @param factor_itens Optional character or factor vector of length
#'   `nrow(item_emb)`. Legacy row grouping variable, often the item scale or
#'   factor. For new code, prefer `x_group`.
#' @param factor_scale Optional character or factor vector of length
#'   `nrow(scale_emb)` when `scale_emb` is supplied, otherwise length
#'   `nrow(item_emb)`. Legacy target labels/groups. For new code, prefer
#'   `y_labels` and `y_group`.
#' @param x_labels Optional character vector of length `nrow(item_emb)`.
#'   Human-readable labels for rows in the long output. Defaults, in order, to
#'   `item_text`, row names of `item_emb`, or generated ids.
#' @param y_labels Optional character vector of length `nrow(scale_emb)` (or
#'   `nrow(item_emb)` in symmetric comparisons). Human-readable labels for
#'   targets in the long output. Defaults, in order, to `factor_scale`, row
#'   names of `scale_emb`, or generated ids.
#' @param x_group Optional character or factor vector of length `nrow(item_emb)`.
#'   Group labels for rows, such as instrument, domain, or scale. Defaults to
#'   `factor_itens` when supplied.
#' @param y_group Optional character or factor vector of length `nrow(scale_emb)`
#'   (or `nrow(item_emb)` in symmetric comparisons). Group labels for targets,
#'   such as instrument, domain, or scale. Defaults to `factor_scale` when
#'   supplied.
#' @param x_type,y_type Length-one character labels describing the row and
#'   column embedding sets, for example `"item"`, `"scale"`, or `"definition"`.
#'   These labels are copied into `cosim_long$x_type` and `cosim_long$y_type`
#'   and are also used when generated ids are needed.
#' @param x_id,y_id Optional stable ids for rows and columns. `x_id` must have
#'   length `nrow(item_emb)`. `y_id` must have length `nrow(scale_emb)`, or
#'   `nrow(item_emb)` in symmetric comparisons. Defaults to row names or
#'   generated ids, then duplicated ids are made unique.
#' @param drop_self Logical scalar. If `TRUE` and the comparison is symmetric
#'   (`scale_emb = NULL`, or row and column ids are identical), diagonal
#'   self-similarities are set to `NA`.
#' @param make_plot Logical scalar. If `TRUE`, `plot_dist` contains a ggplot
#'   histogram/density plot of the finite cosine coefficients; if `FALSE`,
#'   `plot_dist` is `NULL`.
#'
#' @return A list with:
#' \describe{
#'   \item{cosim_mat}{Wide data frame. It starts with `item_text` and `scale`
#'   for backwards compatibility, followed by one cosine column per target.}
#'   \item{cosim_long}{Long pair table with row/column ids, labels, groups,
#'   types, and `cosim`.}
#'   \item{cosim_matrix}{Raw numeric cosine similarity matrix.}
#'   \item{plot_dist}{A ggplot distribution plot, or `NULL` when
#'   `make_plot = FALSE`.}
#' }
#'
#' @details
#' The cosine similarity matrix is computed with `text2vec::sim2(method =
#' "cosine", norm = "l2")`. For row embedding \eqn{x_i} and target embedding
#' \eqn{y_j}, the cosine similarity is
#'
#' \deqn{\cos(x_i, y_j) =
#' \frac{x_i^\top y_j}{\|x_i\|_2 \|y_j\|_2}.}
#'
#' The function also summarizes each row's similarity profile across all
#' targets:
#' \describe{
#'   \item{`best_target_factor`, `second_best_target_factor`}{labels of the two
#'   most similar targets.}
#'   \item{`best_target_id`, `second_best_target_id`}{ids of the two most
#'   similar targets.}
#'   \item{`complexity`}{Hoffman's complexity index,
#'   \eqn{(\sum_j z_j^2)^2 / \sum_j z_j^4}, computed over the row profile
#'   \eqn{z}. Larger values indicate a more diffuse profile across targets.}
#'   \item{`sparsity`}{Hoyer's sparsity,
#'   \eqn{(\sqrt{p} - \|z\|_1 / \|z\|_2) / (\sqrt{p} - 1)}, where \eqn{p} is
#'   the number of finite target similarities. Values closer to 1 indicate a
#'   more concentrated profile.}
#'   \item{`within_sd`}{standard deviation of the row's target similarities.}
#' }
#'
#' The legacy arguments `item_text`, `factor_itens`, and `factor_scale` are kept
#' so older item-scale scripts continue to work. For new analyses, prefer the
#' more general `x_labels`, `y_labels`, `x_group`, `y_group`, `x_type`, and
#' `y_type` arguments.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' item_emb  <- matrix(runif(60, -1, 1), nrow = 6, ncol = 10)
#' scale_emb <- matrix(runif(50, -1, 1), nrow = 5, ncol = 10)
#'
#' out <- cosim_itens_scales(
#'   item_emb = item_emb,
#'   scale_emb = scale_emb,
#'   item_text = paste("item", 1:6),
#'   factor_itens = c("E", "N", "O", "A", "C", "O"),
#'   factor_scale = c("O", "C", "E", "A", "N")
#' )
#'
#' out$cosim_mat
#' out$cosim_long
#'
#' # Symmetric scale-scale similarity
#' ss <- cosim_itens_scales(
#'   item_emb = scale_emb,
#'   x_type = "scale",
#'   y_type = "scale",
#'   drop_self = TRUE
#' )
#' }
#'
#' @import text2vec
#' @importFrom dplyr bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline
#' @importFrom ggplot2 annotate scale_x_continuous labs theme_minimal after_stat
#' @export
cosim_itens_scales <- function(
    item_emb,
    scale_emb = NULL,
    item_text = NULL,
    factor_itens = NULL,
    factor_scale = NULL,
    x_labels = NULL,
    y_labels = NULL,
    x_group = NULL,
    y_group = NULL,
    x_type = "item",
    y_type = "scale",
    x_id = NULL,
    y_id = NULL,
    drop_self = FALSE,
    make_plot = TRUE) {

  x <- embeddcv_as_embedding_matrix(item_emb, "item_emb")
  same_set <- is.null(scale_emb)
  y <- if (same_set) x else embeddcv_as_embedding_matrix(scale_emb, "scale_emb")

  if (ncol(x) != ncol(y)) {
    stop(
      "`item_emb` and `scale_emb` must have the same number of embedding dimensions.",
      call. = FALSE
    )
  }

  # Legacy compatibility: when ids and rownames are absent, fall back to the
  # legacy label vectors so cosine columns keep their old names (e.g. the
  # factor_scale values), and cosim_mat[, factor_scale] keeps working.
  x_id_fallback <- rownames(x)
  if (is.null(x_id_fallback) && !is.null(item_text)) x_id_fallback <- item_text
  y_id_fallback <- rownames(y)
  if (is.null(y_id_fallback) && !is.null(factor_scale) && !same_set) {
    y_id_fallback <- factor_scale
  }

  x_id <- embeddcv_vector_or_default(x_id, x_id_fallback, nrow(x), paste0(x_type, "_"))
  y_id <- embeddcv_vector_or_default(y_id, y_id_fallback, nrow(y), paste0(y_type, "_"))

  x_id <- make.unique(as.character(x_id), sep = "__")
  y_id <- make.unique(as.character(y_id), sep = "__")
  rownames(x) <- x_id
  rownames(y) <- y_id

  x_labels <- embeddcv_vector_or_default(x_labels, item_text, nrow(x), x_id)
  y_labels <- embeddcv_vector_or_default(y_labels, factor_scale, nrow(y), y_id)
  x_group <- embeddcv_vector_or_default(x_group, factor_itens, nrow(x), x_labels)
  y_group <- embeddcv_vector_or_default(y_group, factor_scale, nrow(y), y_labels)

  x_labels <- as.character(x_labels)
  y_labels <- as.character(y_labels)
  x_group <- as.character(x_group)
  y_group <- as.character(y_group)

  cosim_matrix <- as.matrix(text2vec::sim2(
    x = x,
    y = y,
    method = "cosine",
    norm = "l2"
  ))
  dimnames(cosim_matrix) <- list(x_id, y_id)

  if (isTRUE(drop_self) && nrow(cosim_matrix) == ncol(cosim_matrix) &&
      identical(x_id, y_id)) {
    diag(cosim_matrix) <- NA_real_
  }

  cosim_df <- as.data.frame(cosim_matrix, check.names = FALSE)

  best_two <- t(apply(cosim_matrix, 1, function(z) {
    if (all(is.na(z))) {
      return(c(
        best_target_factor = NA_character_,
        second_best_target_factor = NA_character_,
        best_target_id = NA_character_,
        second_best_target_id = NA_character_
      ))
    }

    ord <- order(z, decreasing = TRUE, na.last = NA)
    c(
      best_target_factor = y_labels[ord[1]],
      second_best_target_factor = if (length(ord) > 1) y_labels[ord[2]] else NA_character_,
      best_target_id = y_id[ord[1]],
      second_best_target_id = if (length(ord) > 1) y_id[ord[2]] else NA_character_
    )
  }))

  cosim_mat <- dplyr::bind_cols(
    data.frame(
      item_text = x_labels,
      scale = x_group,
      stringsAsFactors = FALSE
    ),
    cosim_df,
    data.frame(
      best_target_factor = best_two[, "best_target_factor"],
      second_best_target_factor = best_two[, "second_best_target_factor"],
      best_target_id = best_two[, "best_target_id"],
      second_best_target_id = best_two[, "second_best_target_id"],
      complexity = apply(cosim_matrix, 1, hoffman_complexity),
      sparsity = apply(cosim_matrix, 1, hoyer_sparsity),
      within_sd = apply(cosim_matrix, 1, stats::sd, na.rm = TRUE),
      x_id = x_id,
      x_label = x_labels,
      x_group = x_group,
      x_type = x_type,
      stringsAsFactors = FALSE
    )
  )

  cosim_long <- data.frame(
    x_id = rep(x_id, times = length(y_id)),
    y_id = rep(y_id, each = length(x_id)),
    cosim = as.vector(cosim_matrix),
    stringsAsFactors = FALSE
  )
  cosim_long$x_label <- x_labels[match(cosim_long$x_id, x_id)]
  cosim_long$x_group <- x_group[match(cosim_long$x_id, x_id)]
  cosim_long$x_type <- x_type
  cosim_long$y_label <- y_labels[match(cosim_long$y_id, y_id)]
  cosim_long$y_group <- y_group[match(cosim_long$y_id, y_id)]
  cosim_long$y_type <- y_type
  cosim_long <- cosim_long[
    c("x_id", "x_label", "x_group", "x_type",
      "y_id", "y_label", "y_group", "y_type", "cosim")
  ]

  plot_dist <- NULL
  if (isTRUE(make_plot)) {
    plot_dist <- embeddcv_cosim_distribution_plot(cosim_long$cosim)
  }

  list(
    cosim_mat = cosim_mat,
    cosim_long = cosim_long,
    cosim_matrix = cosim_matrix,
    plot_dist = plot_dist
  )
}

#' Compute all pairwise cosine similarities among named embedding sets
#'
#' Convenience wrapper around `cosim_itens_scales()` for producing all pairwise
#' comparisons among named embedding sets. It is useful when an analysis needs
#' item-item, item-scale, item-definition, scale-scale, scale-definition, and
#' definition-definition similarities from the same set of embeddings.
#'
#' @param embeddings Named list of numeric matrices or data frames. Each element
#'   is an embedding set with rows as units and columns as embedding dimensions.
#'   All elements must have the same number of columns. The names of the list
#'   become set labels such as `"item"`, `"scale"`, and `"definition"`.
#' @param labels Optional named list of character vectors. Names should match
#'   `names(embeddings)`. Each vector must have length equal to the number of
#'   rows in the corresponding embedding matrix and is passed to
#'   `cosim_itens_scales()` as `x_labels` or `y_labels`.
#' @param groups Optional named list of character or factor vectors. Names
#'   should match `names(embeddings)`. Each vector must have length equal to the
#'   number of rows in the corresponding embedding matrix and is passed to
#'   `cosim_itens_scales()` as `x_group` or `y_group`.
#' @param include_same Logical scalar. If `TRUE`, include within-set
#'   comparisons such as `item_item` and `scale_scale`. If `FALSE`, only
#'   between-set comparisons are returned.
#' @param drop_self Logical scalar. If `TRUE`, diagonal self-similarities in
#'   within-set comparisons are set to `NA`.
#' @param make_plot Logical scalar passed to `cosim_itens_scales()`. If `TRUE`,
#'   each returned comparison includes a distribution plot in `plot_dist`.
#'
#' @details
#' If `embeddings` has names `c("item", "scale", "definition")`, the default
#' `include_same = TRUE` returns the upper-triangular set of comparisons:
#' `item_item`, `item_scale`, `item_definition`, `scale_scale`,
#' `scale_definition`, and `definition_definition`. Within-set comparisons use
#' `drop_self = TRUE` by default, so diagonal self-similarities are set to `NA`.
#'
#' @return A named list of `cosim_itens_scales()` outputs. Each element contains
#'   `cosim_mat`, `cosim_long`, `cosim_matrix`, and `plot_dist`.
#'
#' @examples
#' \dontrun{
#' embeddings <- list(
#'   item = item_emb,
#'   scale = scale_emb,
#'   definition = definition_emb
#' )
#' sims <- cosim_all_pairs(embeddings, make_plot = FALSE)
#' names(sims)
#' sims$scale_definition$cosim_long
#' }
#' @export
cosim_all_pairs <- function(
    embeddings,
    labels = NULL,
    groups = NULL,
    include_same = TRUE,
    drop_self = TRUE,
    make_plot = FALSE) {

  if (is.null(names(embeddings)) || any(names(embeddings) == "")) {
    stop("`embeddings` must be a named list.", call. = FALSE)
  }

  set_names <- names(embeddings)
  pair_index <- expand.grid(x = seq_along(set_names), y = seq_along(set_names))
  pair_index <- pair_index[pair_index$x <= pair_index$y, , drop = FALSE]

  if (!isTRUE(include_same)) {
    pair_index <- pair_index[pair_index$x != pair_index$y, , drop = FALSE]
  }

  out <- vector("list", nrow(pair_index))
  out_names <- character(nrow(pair_index))

  for (i in seq_len(nrow(pair_index))) {
    x_name <- set_names[pair_index$x[i]]
    y_name <- set_names[pair_index$y[i]]
    out_names[i] <- paste(x_name, y_name, sep = "_")

    out[[i]] <- cosim_itens_scales(
      item_emb = embeddings[[x_name]],
      scale_emb = embeddings[[y_name]],
      x_labels = labels[[x_name]],
      y_labels = labels[[y_name]],
      x_group = groups[[x_name]],
      y_group = groups[[y_name]],
      x_type = x_name,
      y_type = y_name,
      drop_self = isTRUE(drop_self) && x_name == y_name,
      make_plot = make_plot
    )
  }

  names(out) <- out_names
  out
}

embeddcv_as_embedding_matrix <- function(x, arg_name) {
  out <- as.matrix(x)
  storage.mode(out) <- "double"

  if (!is.numeric(out)) {
    stop("`", arg_name, "` must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(out))) {
    stop("`", arg_name, "` contains non-finite values.", call. = FALSE)
  }

  out
}

embeddcv_vector_or_default <- function(x, fallback, n, prefix) {
  out <- x
  if (is.null(out)) out <- fallback
  if (is.null(out)) {
    out <- if (length(prefix) == 1) paste0(prefix, seq_len(n)) else prefix
  }
  if (length(out) != n) {
    stop("Metadata vectors must have length ", n, ".", call. = FALSE)
  }
  out
}

embeddcv_cosim_distribution_plot <- function(cosim) {
  cosim <- cosim[is.finite(cosim)]
  pct <- stats::quantile(cosim, probs = seq(.1, .9, .1), na.rm = TRUE)
  mean_val <- mean(cosim, na.rm = TRUE)
  sd_val <- stats::sd(cosim, na.rm = TRUE)

  subtitle <- sprintf(
    "n = %d  |  mean = %.3f  |  sd = %.3f  |  range = [%.3f, %.3f]",
    length(cosim), mean_val, sd_val, min(cosim), max(cosim)
  )

  long <- data.frame(cosim = cosim)

  ggplot2::ggplot(long, ggplot2::aes(x = cosim)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(.data$density)),
      binwidth = 0.02,
      colour = "white",
      fill = "salmon",
      alpha = 0.85
    ) +
    ggplot2::geom_density(
      colour = "#c0392b",
      linewidth = 0.8,
      adjust = 1.2
    ) +
    ggplot2::geom_vline(
      xintercept = pct,
      linetype = "dashed",
      colour = "grey30",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val,
      linetype = "solid",
      colour = "#2c3e50",
      linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = pct,
      y = Inf,
      label = names(pct),
      angle = 90,
      vjust = -0.4,
      hjust = 1.1,
      size = 2.8,
      colour = "grey30"
    ) +
    ggplot2::annotate(
      "text",
      x = mean_val,
      y = Inf,
      label = paste0("mean\n", round(mean_val, 3)),
      vjust = -0.3,
      hjust = -0.15,
      size = 2.8,
      colour = "#2c3e50"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(0.10)) +
    ggplot2::labs(
      title = "Distribution of Cosine Similarity Coefficients",
      subtitle = subtitle,
      x = "Cosine similarity",
      y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Hoyer's Sparsity Measure
#'
#' Computes Hoyer's sparsity metric, which quantifies the level of sparsity in a
#' numeric vector. The value ranges from 0 (dense) to 1 (maximally sparse).
#'
#' @details
#' For a vector \eqn{x} of length \eqn{n}, Hoyer's sparsity is
#'
#' \deqn{\frac{\sqrt{n} - \|x\|_1 / \|x\|_2}{\sqrt{n} - 1}.}
#'
#' The function removes non-finite values before computing the index. If the
#' vector has length 0 or 1 after filtering, or if the L2 norm is zero, the
#' function returns 0.
#'
#' @param x Numeric vector.
#' @return Numeric value between 0 and 1. Returns 0 if the L2 norm is zero.
#' @export
hoyer_sparsity <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n <= 1) return(0)
  l1_norm <- sum(abs(x))
  l2_norm <- sqrt(sum(x^2))
  if (l2_norm == 0) return(0)
  (sqrt(n) - (l1_norm / l2_norm)) / (sqrt(n) - 1)
}

#' Hoffman's Complexity Index
#'
#' Computes Hoffman's complexity index for a numeric vector, often used for
#' factor loadings or correlation patterns.
#'
#' @details
#' For a finite vector \eqn{x}, Hoffman's complexity index is
#'
#' \deqn{\frac{(\sum_i x_i^2)^2}{\sum_i x_i^4}.}
#'
#' Values near 1 indicate that one element dominates the profile. Larger values
#' indicate that the profile is spread across more elements. Non-finite values
#' are removed before the calculation.
#'
#' @param x Numeric vector.
#' @return Numeric complexity value. Returns `NA` if the fourth-power sum is
#'   zero.
#' @export
hoffman_complexity <- function(x) {
  x <- x[is.finite(x)]
  sum_sq <- sum(x^2)
  sum_quad <- sum(x^4)
  if (sum_quad == 0) return(NA_real_)
  (sum_sq^2) / sum_quad
}
