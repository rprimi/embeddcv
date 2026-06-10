#' Compute jingle-jangle similarity thresholds
#'
#' Computes the semantic similarity cutoffs used to define jingle and jangle
#' fallacy regions. The default settings reproduce the threshold rule used by
#' Wulff & Mata (2025): a "low" cutoff at the 80th percentile and a "high"
#' cutoff at the 99th percentile.
#'
#' @details
#' Let \eqn{S} be the empirical distribution of scale-scale similarities and
#' \eqn{L} the empirical distribution of label-label similarities. The function
#' returns:
#'
#' \deqn{s_{low} = Q_S(1 - 1 / \textrm{min\_constructs})}
#' \deqn{l_{low} = Q_L(1 - 1 / \textrm{min\_constructs})}
#' \deqn{s_{high} = Q_S(1 - 1 / \textrm{max\_constructs})}
#' \deqn{l_{high} = Q_L(1 - 1 / \textrm{max\_constructs})}
#'
#' With the defaults `min_constructs = 5` and `max_constructs = 100`, these are
#' `q80` and `q99`. The naming follows the fallacy logic: `scale_low` and
#' `label_low` are lower thresholds relative to the stricter high thresholds,
#' not lower-quartile thresholds.
#'
#' @param scale_cos Numeric scale-scale similarity matrix or numeric vector of
#'   scale-scale similarities. A matrix is typically produced by
#'   `cosine_matrix()` and optionally `rescale_similarity()`. Matrix diagonal
#'   values are included unless they have already been removed or set to `NA`;
#'   pass the same object used by the analysis pipeline for exact reproduction.
#' @param label_cos Numeric label-label or definition-definition similarity
#'   matrix, or numeric vector of label similarities. This is typically produced
#'   by `cosine_matrix()` on label/definition embeddings.
#' @param min_constructs Numeric scalar denominator for the low cutoff.
#'   Wulff & Mata (2025)
#'   used `5`, yielding `1 - 1/5 = .80`.
#' @param max_constructs Numeric scalar denominator for the high cutoff.
#'   Wulff & Mata (2025) used `100`, yielding `1 - 1/100 = .99`.
#'
#' @return Named numeric vector with `scale_low`, `label_low`, `scale_high`,
#'   and `label_high`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' scale_cos <- c(.10, .30, .60, .75, .90, .95)
#' label_cos <- c(.05, .20, .55, .70, .88, .93)
#' jingle_jangle_thresholds(scale_cos, label_cos)
#' @export
jingle_jangle_thresholds <- function(
    scale_cos,
    label_cos,
    min_constructs = 5,
    max_constructs = 100) {

  c(
    scale_low = unname(stats::quantile(c(scale_cos), 1 - 1 / min_constructs, na.rm = TRUE)),
    label_low = unname(stats::quantile(c(label_cos), 1 - 1 / min_constructs, na.rm = TRUE)),
    scale_high = unname(stats::quantile(c(scale_cos), 1 - 1 / max_constructs, na.rm = TRUE)),
    label_high = unname(stats::quantile(c(label_cos), 1 - 1 / max_constructs, na.rm = TRUE))
  )
}

#' Build scale-label pair data for jingle-jangle analysis
#'
#' Converts aligned scale-scale and label-label similarity matrices into a long
#' pair table. Each row corresponds to a pair of scales and contains both the
#' similarity between the scales and the similarity between the labels attached
#' to those scales. This is the pair-construction step needed for the automated
#' jingle-jangle detection approach described by Wulff & Mata (2025).
#'
#' @details
#' If `label_by_scale` is supplied, it maps each scale id to the corresponding
#' label id in `label_cos`. This is important when item-derived scale ids and
#' definition/label ids are not identical. For a scale pair \eqn{(i,j)}, the
#' function returns:
#'
#' \deqn{\textrm{scale\_cos}_{ij} = S_{ij}}
#'
#' and
#'
#' \deqn{\textrm{label\_cos}_{ij} = L_{m(i),m(j)},}
#'
#' where \eqn{m(i)} is the label id mapped to scale \eqn{i}.
#'
#' @param scale_cos Numeric square scale-scale similarity matrix. Row and column
#'   names must be identical scale ids in the same order. This matrix is often
#'   produced by `cosine_matrix(scale_embeddings)` followed by
#'   `rescale_similarity()` when clustering should use a 0-1 range.
#' @param label_cos Numeric square label-label similarity matrix. Row and column
#'   names must contain all label ids referenced by `label_by_scale`. This
#'   matrix is often produced by `cosine_matrix(label_embeddings)`.
#' @param label_by_scale Optional named character vector mapping scale ids to
#'   label ids. `names(label_by_scale)` must include every row name in
#'   `scale_cos`; the values must be row/column names in `label_cos`. If `NULL`,
#'   scale ids are assumed to be identical to label ids.
#' @param label_fun Function that accepts a character vector of scale ids and
#'   returns display construct labels of the same length. For example, the
#'   default `scale_label_from_scale()` turns `"pers_inv_social_boldness"` into
#'   `"inv_social_boldness"`.
#' @param instrument_fun Function that accepts a character vector of scale ids
#'   and returns display instrument names of the same length. The default
#'   `instrument_from_scale()` turns `"pers_inv_social_boldness"` into
#'   `"pers"`.
#' @param upper_only Logical scalar. If `TRUE`, keep only unique unordered scale
#'   pairs (`i < j`). If `FALSE`, all ordered pairs are returned.
#'
#' @return Data frame with scale ids, display labels, instrument labels, mapped
#'   label ids, `label_cos`, and `scale_cos`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
jingle_jangle_pairs <- function(
    scale_cos,
    label_cos,
    label_by_scale = NULL,
    label_fun = scale_label_from_scale,
    instrument_fun = instrument_from_scale,
    upper_only = TRUE) {

  scale_cos <- as.matrix(scale_cos)
  label_cos <- as.matrix(label_cos)

  scales <- rownames(scale_cos)
  if (is.null(scales) || is.null(colnames(scale_cos))) {
    stop("`scale_cos` must have row and column names.", call. = FALSE)
  }
  if (!identical(scales, colnames(scale_cos))) {
    stop("`scale_cos` row names and column names must be aligned.", call. = FALSE)
  }

  if (is.null(label_by_scale)) {
    label_by_scale <- scales
    names(label_by_scale) <- scales
  }

  label_by_scale <- as.character(label_by_scale[scales])
  if (anyNA(label_by_scale)) {
    stop("`label_by_scale` must map every scale in `scale_cos`.", call. = FALSE)
  }
  if (!all(label_by_scale %in% rownames(label_cos)) ||
      !all(label_by_scale %in% colnames(label_cos))) {
    stop("Every mapped label must exist in `label_cos` row and column names.", call. = FALSE)
  }

  idx <- expand.grid(i = seq_along(scales), j = seq_along(scales))
  if (isTRUE(upper_only)) {
    idx <- idx[idx$i < idx$j, , drop = FALSE]
  }

  scale_1 <- scales[idx$i]
  scale_2 <- scales[idx$j]
  label_1_id <- label_by_scale[idx$i]
  label_2_id <- label_by_scale[idx$j]

  out <- data.frame(
    scale_1 = scale_1,
    instrument_1 = instrument_fun(scale_1),
    label_1 = label_fun(scale_1),
    scale_2 = scale_2,
    instrument_2 = instrument_fun(scale_2),
    label_2 = label_fun(scale_2),
    label_id_1 = label_1_id,
    label_id_2 = label_2_id,
    label_cos = label_cos[cbind(label_1_id, label_2_id)],
    scale_cos = scale_cos[cbind(scale_1, scale_2)],
    stringsAsFactors = FALSE
  )

  out
}

#' Classify scale pairs as jingle or jangle fallacies
#'
#' Adds fallacy labels to a pair table created by `jingle_jangle_pairs()`.
#' This implements the automated jingle-jangle detection rule described by
#' Wulff & Mata (2025): jingle fallacies are pairs with similar labels but
#' dissimilar scale content, and jangle fallacies are pairs with dissimilar
#' labels but similar scale content.
#'
#' @details
#' Each pair is classified using the same rules as `score_label_solution()`:
#'
#' \deqn{\textrm{jingle} \iff \textrm{scale\_cos} < s_{low}
#' \ \textrm{and}\ \textrm{label\_cos} > l_{high}}
#'
#' \deqn{\textrm{jangle} \iff \textrm{scale\_cos} > s_{high}
#' \ \textrm{and}\ \textrm{label\_cos} < l_{low}.}
#'
#' A jingle fallacy means labels are semantically similar while the scales are
#' semantically dissimilar. A jangle fallacy means labels are semantically
#' dissimilar while the scales are semantically similar.
#'
#' @param pair_table Data frame or tibble, usually the output of
#'   `jingle_jangle_pairs()`. It must contain numeric columns `scale_cos` and
#'   `label_cos`, plus display columns `label_1`, `label_2`, `instrument_1`,
#'   and `instrument_2`.
#' @param thresholds Named numeric vector, usually from
#'   `jingle_jangle_thresholds()`. It must contain `scale_low`, `label_low`,
#'   `scale_high`, and `label_high`.
#' @param verbose Logical scalar. If `TRUE`, prints the number of jingle
#'   fallacies, jangle fallacies, and non-fallacy pairs.
#'
#' @return `pair_table` with added columns:
#' \describe{
#'   \item{type}{`"Jingle fallacy"`, `"Jangle fallacy"`, or `"nothing"`.}
#'   \item{cos_diff}{Absolute difference between scale and label similarity.}
#'   \item{label}{Short display label for plots.}
#' }
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
classify_jingle_jangle <- function(pair_table, thresholds, verbose = TRUE) {
  required <- c("scale_low", "label_low", "scale_high", "label_high")
  if (!all(required %in% names(thresholds))) {
    stop("`thresholds` must include: ", paste(required, collapse = ", "), call. = FALSE)
  }

  out <- pair_table
  out$type <- ifelse(
    out$scale_cos < thresholds[["scale_low"]] & out$label_cos > thresholds[["label_high"]],
    "Jingle fallacy",
    ifelse(
      out$scale_cos > thresholds[["scale_high"]] & out$label_cos < thresholds[["label_low"]],
      "Jangle fallacy",
      "nothing"
    )
  )
  out$cos_diff <- abs(out$scale_cos - out$label_cos)
  out$scale_construct_i_short <- short_text(out$label_1, 18)
  out$scale_construct_j_short <- short_text(out$label_2, 18)
  out$inventory_i <- out$instrument_1
  out$inventory_j <- out$instrument_2
  out$label <- paste0(
    capitalize_first(out$scale_construct_i_short), " (", out$inventory_i, ") - ",
    capitalize_first(out$scale_construct_j_short), " (", out$inventory_j, ")"
  )
  if (isTRUE(verbose)) {
    embeddcv_message_jj_counts(out, prefix = "Jingle-jangle classification")
  }
  out
}

#' Evaluate jingle-jangle sensitivity over cutoff grids
#'
#' Recomputes jingle and jangle counts over a grid of threshold rules. This is
#' the data-preparation step behind robustness plots that ask whether the number
#' of fallacies is stable across stricter or looser semantic cutoff definitions.
#'
#' @details
#' For each combination of `lows` and `highs`, thresholds are computed with
#' `jingle_jangle_thresholds(min_constructs = lows[i], max_constructs =
#' highs[j])`. Thus each low denominator \eqn{a} corresponds to a quantile
#' \eqn{1 - 1/a}, and each high denominator \eqn{b} corresponds to
#' \eqn{1 - 1/b}.
#'
#' The default grid mirrors the sensitivity grid in Wulff & Mata (2025):
#' `lows = 1:10` evaluates low cutoffs from \eqn{q_0} through \eqn{q_{90}},
#' and `highs = seq(50, 250, 20)` evaluates high cutoffs from approximately
#' \eqn{q_{98}} through \eqn{q_{99.6}}.
#'
#' @param pair_table Data frame or tibble, usually the output of
#'   `jingle_jangle_pairs()`. It must contain numeric `scale_cos` and
#'   `label_cos` columns because it is repeatedly passed to
#'   `classify_jingle_jangle()`.
#' @param scale_cos Numeric scale-scale similarity matrix or vector used to
#'   compute scale cutoffs. Use the same object used to create `pair_table`.
#' @param label_cos Numeric label-label similarity matrix or vector used to
#'   compute label cutoffs. Use the same object used to create `pair_table`.
#' @param lows Numeric vector of low-cutoff denominators. Each value must be
#'   greater than 1; for example, `5` means the low cutoff is the 80th
#'   percentile.
#' @param highs Numeric vector of high-cutoff denominators. Each value must be
#'   greater than 1; for example, `100` means the high cutoff is the 99th
#'   percentile.
#'
#' @return Data frame with one row per grid cell and columns for the denominator
#'   values, grid coordinates (`x`, `y`), jingle/jangle counts, and rounded
#'   cutoff values.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
jingle_jangle_threshold_grid <- function(
    pair_table,
    scale_cos,
    label_cos,
    lows = 1:10,
    highs = seq(50, 250, 20)) {

  grid <- expand.grid(lows = lows, highs = highs)
  out <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    th <- jingle_jangle_thresholds(
      scale_cos = scale_cos,
      label_cos = label_cos,
      min_constructs = grid$lows[i],
      max_constructs = grid$highs[i]
    )
    tmp <- classify_jingle_jangle(pair_table, th, verbose = FALSE)
    out[[i]] <- data.frame(
      lows = grid$lows[i],
      highs = grid$highs[i],
      x = match(grid$lows[i], lows),
      y = match(grid$highs[i], highs),
      jingle = sum(tmp$type == "Jingle fallacy", na.rm = TRUE),
      jangle = sum(tmp$type == "Jangle fallacy", na.rm = TRUE),
      scale_low = round(unname(th["scale_low"]), 2),
      scale_high = round(unname(th["scale_high"]), 2),
      label_low = round(unname(th["label_low"]), 2),
      label_high = round(unname(th["label_high"]), 2)
    )
  }

  do.call(rbind, out)
}

#' Plot jingle-jangle cutoff sensitivity grids
#'
#' Draws the cutoff-sensitivity display for a table returned by
#' `jingle_jangle_threshold_grid()`. Each panel is a tile grid whose rows and
#' columns are the high- and low-threshold denominators used to classify
#' jingle and jangle fallacies. The default panels show how the number of
#' jingle fallacies, number of jangle fallacies, and the corresponding
#' similarity cutoffs change as the assumed minimum and maximum number of
#' constructs vary.
#'
#' @details
#' Let \eqn{a} be a value in `lows` and \eqn{b} be a value in `highs`.
#' `jingle_jangle_threshold_grid()` converts those denominator rules into
#' quantiles:
#' \deqn{c_{\mathrm{low}} = Q(1 - 1/a)}
#' \deqn{c_{\mathrm{high}} = Q(1 - 1/b)}
#'
#' The low and high cutoffs are computed separately for the scale-similarity
#' distribution and the label-similarity distribution. A scale pair is counted
#' as a jingle fallacy when label similarity is at or above its high cutoff and
#' scale similarity is at or below its low cutoff. A scale pair is counted as a
#' jangle fallacy when scale similarity is at or above its high cutoff and label
#' similarity is at or below its low cutoff. This is the same cutoff-sensitivity
#' logic used by Wulff & Mata (2025) to inspect whether conclusions depend on
#' the chosen construct-count assumptions.
#'
#' @param threshold_grid Data frame returned by
#'   `jingle_jangle_threshold_grid()`. It must contain `lows`, `highs`, `x`,
#'   `y`, `jingle`, `jangle`, `scale_low`, `scale_high`, `label_low`, and
#'   `label_high`.
#' @param panels Character vector naming the panels to draw. Available panels
#'   are `"jingle"`, `"jangle"`, `"scale_low"`, `"scale_high"`,
#'   `"label_low"`, and `"label_high"`.
#' @param panel_labels Optional named character vector used to relabel panel
#'   titles. Names must match values in `panels`.
#' @param output_file Optional path. If supplied, a file is opened and the plot
#'   is written to disk. Files ending in `.pdf` use `grDevices::pdf()`; all
#'   other extensions use `grDevices::png()`.
#' @param width,height Numeric scalars. Device size in inches.
#' @param res Numeric scalar. PNG resolution in pixels per inch.
#' @param palette Character scalar. Tile palette. Options are `"viridis"`
#'   (default), `"wulff_mata"`, `"cividis"`, `"magma"`, `"plasma"`,
#'   `"inferno"`, `"okabe_ito"`, and `"blue_gold"`. The default uses the same
#'   viridis endpoint as the cutoff-sensitivity display in Wulff & Mata (2025).
#' @param n_colours Integer scalar. Number of discrete colors used to map each
#'   panel's values. The default `20` follows the Wulff & Mata (2025)
#'   cutoff-sensitivity display.
#' @param text_col Character scalar. Cell-label color.
#' @param border_col Character scalar or `NA`. Tile border color.
#' @param digits Integer scalar. Number of decimal places for cutoff-value
#'   panels. Count panels are printed as integers.
#' @param text_cex,axis_cex,title_cex Numeric scalars passed to base graphics
#'   text drawing.
#' @param label_cells Logical scalar. If `TRUE`, writes each cell value inside
#'   the tile.
#' @param mar Numeric vector of length four passed to `graphics::par(mar = )`
#'   for each panel.
#'
#' @return Invisibly returns `threshold_grid`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
plot_jingle_jangle_cutoffs <- function(
    threshold_grid,
    panels = c("jingle", "jangle", "scale_low", "scale_high", "label_low", "label_high"),
    panel_labels = NULL,
    output_file = NULL,
    width = 10,
    height = 6.5,
    res = 300,
    palette = "viridis",
    n_colours = 20,
    text_col = "white",
    border_col = NA,
    digits = 2,
    text_cex = .6,
    axis_cex = .8,
    title_cex = 1,
    label_cells = TRUE,
    mar = c(4, 4.4, 2.8, .8)) {

  required <- c("lows", "highs", "x", "y", "jingle", "jangle",
                "scale_low", "scale_high", "label_low", "label_high")
  missing <- setdiff(required, names(threshold_grid))
  if (length(missing) > 0) {
    stop("`threshold_grid` is missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  available_panels <- c("jingle", "jangle", "scale_low", "scale_high",
                        "label_low", "label_high")
  panels <- match.arg(panels, available_panels, several.ok = TRUE)

  default_labels <- c(
    jingle = "Jingle fallacies",
    jangle = "Jangle fallacies",
    scale_low = "Scale low cutoff",
    scale_high = "Scale high cutoff",
    label_low = "Label low cutoff",
    label_high = "Label high cutoff"
  )
  if (!is.null(panel_labels)) {
    default_labels[names(panel_labels)] <- panel_labels
  }

  lows <- sort(unique(threshold_grid$lows))
  highs <- sort(unique(threshold_grid$highs))
  if (!is.numeric(n_colours) || length(n_colours) != 1 || n_colours < 2) {
    stop("`n_colours` must be a numeric scalar greater than 1.", call. = FALSE)
  }
  n_colours <- as.integer(n_colours)
  pal <- embeddcv_relabel_palette(n_colours, palette = palette, begin = 0, end = .9)

  if (!is.null(output_file)) {
    if (grepl("\\.pdf$", output_file, ignore.case = TRUE)) {
      grDevices::pdf(output_file, width = width, height = height)
    } else {
      grDevices::png(output_file, width = width, height = height,
                     units = "in", res = res)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  panel_count <- length(panels)
  n_col <- ceiling(sqrt(panel_count))
  n_row <- ceiling(panel_count / n_col)
  graphics::par(mfrow = c(n_row, n_col), mar = mar, xaxs = "i", yaxs = "i")

  for (panel in panels) {
    values <- matrix(NA_real_, nrow = length(highs), ncol = length(lows),
                     dimnames = list(highs, lows))
    for (i in seq_len(nrow(threshold_grid))) {
      row_id <- match(threshold_grid$highs[i], highs)
      col_id <- match(threshold_grid$lows[i], lows)
      values[row_id, col_id] <- threshold_grid[[panel]][i]
    }

    z <- values
    z_min <- min(z, na.rm = TRUE)
    z_max <- max(z, na.rm = TRUE)
    if (!is.finite(z_min) || !is.finite(z_max)) {
      z_min <- 0
      z_max <- 1
    }
    if (identical(z_min, z_max)) {
      col_index <- matrix(ceiling(n_colours / 2), nrow = nrow(z), ncol = ncol(z))
    } else {
      col_index <- floor((z - z_min) / (z_max - z_min) * (n_colours - .0001)) + 1L
    }
    col_index[!is.finite(col_index)] <- 1L

    graphics::plot(
      NA,
      xlim = c(.5, length(lows) + .5),
      ylim = c(.5, length(highs) + .5),
      axes = FALSE,
      xlab = "Minimum number of constructs",
      ylab = "Maximum number of constructs"
    )

    for (row_id in seq_along(highs)) {
      for (col_id in seq_along(lows)) {
        graphics::rect(
          col_id - .5,
          row_id - .5,
          col_id + .5,
          row_id + .5,
          col = pal[col_index[row_id, col_id]],
          border = border_col
        )
        if (isTRUE(label_cells)) {
          cell_value <- values[row_id, col_id]
          label <- if (panel %in% c("jingle", "jangle")) {
            format(round(cell_value), trim = TRUE)
          } else {
            format(round(cell_value, digits), nsmall = digits, trim = TRUE)
          }
          graphics::text(col_id, row_id, label, cex = text_cex, col = text_col)
        }
      }
    }

    graphics::axis(1, at = seq_along(lows), labels = lows, cex.axis = axis_cex)
    graphics::axis(2, at = seq_along(highs), labels = highs,
                   cex.axis = axis_cex, las = 2)
    graphics::box()
    graphics::title(default_labels[[panel]], cex.main = title_cex)
  }

  invisible(threshold_grid)
}

#' Plot scale-label similarity scatter with jingle-jangle regions
#'
#' Draws a diagnostic scatterplot of scale similarity against label similarity.
#' Each point is a pair of scales. Points classified as jingle or jangle
#' fallacies are colored; all other points remain in the background cloud.
#'
#' @param pair_table Data frame or tibble with one row per scale pair. It can be
#'   the output of `jingle_jangle_pairs()` or `classify_jingle_jangle()`. It must
#'   contain finite numeric columns `scale_cos` and `label_cos`; if it has no
#'   valid `type` column, the function reclassifies pairs using `thresholds` or
#'   `threshold_rule`.
#' @param thresholds Optional named numeric vector from
#'   `jingle_jangle_thresholds()`. It must contain `scale_low`, `label_low`,
#'   `scale_high`, and `label_high`. When `threshold_rule = "wulff_mata"` and
#'   this is supplied, the table is classified with these thresholds before
#'   plotting. When `threshold_rule = "custom"`, this argument is required.
#' @param threshold_rule Character scalar. Threshold rule used for guides and
#'   highlighted points. `"wulff_mata"` is the default denominator-based rule
#'   implemented by `jingle_jangle_thresholds()`: `1 - 1 / min_constructs` and
#'   `1 - 1 / max_constructs`. `"quartile"` uses the 25th and 75th percentiles
#'   of the plotted pair similarities. `"custom"` uses the supplied
#'   `thresholds`.
#' @details
#' The x-axis is `scale_cos` and the y-axis is `label_cos`. The default
#' `threshold_rule = "wulff_mata"` uses the quantiles defined by
#' `jingle_jangle_thresholds()`: q80 for the low thresholds and q99 for the
#' high thresholds when `min_constructs = 5` and `max_constructs = 100`.
#' `threshold_rule = "quartile"` uses q25 and q75 of the plotted pair table.
#' `threshold_rule = "custom"` uses the values supplied in `thresholds`.
#'
#' Jingle and jangle classes are computed with `classify_jingle_jangle()`.
#' The optional regression line is a linear model \eqn{label\_cos =
#' \beta_0 + \beta_1 scale\_cos + \epsilon} fit by `ggplot2::geom_smooth()`.
#'
#' @param output_file Optional character path. If supplied, the plot is written
#'   with `ggplot2::ggsave()`; if `NULL`, only the ggplot object is returned.
#' @param width,height Numeric scalars. Plot size in inches passed to
#'   `ggplot2::ggsave()` when `output_file` is not `NULL`.
#' @param dpi Numeric scalar. Resolution in pixels per inch passed to
#'   `ggplot2::ggsave()` when `output_file` is not `NULL`.
#' @param point_alpha Numeric scalar between 0 and 1. Alpha transparency for
#'   non-fallacy points.
#' @param highlight_alpha Numeric scalar between 0 and 1. Alpha transparency for
#'   highlighted fallacy points.
#' @param point_size Numeric scalar. Size used for the base cloud of all points.
#' @param highlight_size Numeric scalar. Size used for highlighted jingle and
#'   jangle points.
#'   Defaults to the same size as the background points so changing thresholds
#'   does not visually redraw the scatter cloud.
#' @param highlight_stroke Numeric scalar. Line width for point markers when a
#'   stroked shape is used.
#' @param region_alpha Numeric scalar between 0 and 1. Alpha for the shaded
#'   fallacy regions. The default is `0`
#'   so that threshold changes do not visually create disconnected quadrants;
#'   increase it (for example `.04`) if you want shaded regions.
#' @param show_regression Logical scalar. If `TRUE`, add a linear regression
#'   line.
#' @param regression_colour Character scalar. Color of the regression line.
#' @param regression_linewidth Numeric scalar. Line width of the regression
#'   line.
#' @param palette Character scalar. Color palette for highlighted jingle and
#'   jangle points.
#'   Options are `"wulff_mata"` (default), `"cividis"`, `"viridis"`,
#'   `"magma"`, `"plasma"`, `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#'   The `"wulff_mata"` palette is a blue/gold cividis-based palette.
#' @param show_threshold_note Logical scalar. If `TRUE`, add a plot caption
#'   reporting the quantile rule and cutoff values used in the plot.
#' @param min_constructs,max_constructs Numeric scalar threshold denominators
#'   used when
#'   `threshold_rule = "wulff_mata"` and `thresholds = NULL`. Defaults use
#'   `1 - 1/5` and `1 - 1/100`, i.e. q80 and q99.
#' @return A ggplot object. If `output_file` is not `NULL`, the same plot is
#'   also saved to disk.
#'
#' @examples
#' \dontrun{
#' pairs <- jingle_jangle_pairs(scale_cos, label_cos, label_by_scale)
#' th <- jingle_jangle_thresholds(scale_cos, label_cos)
#'
#' plot_jingle_jangle_scatter(pairs, thresholds = th)
#' plot_jingle_jangle_scatter(pairs, threshold_rule = "quartile",
#'                            palette = "blue_gold")
#' plot_jingle_jangle_scatter(
#'   pairs,
#'   threshold_rule = "custom",
#'   thresholds = c(scale_low = .40, label_low = .40,
#'                  scale_high = .80, label_high = .80),
#'   region_alpha = .03,
#'   show_threshold_note = TRUE
#' )
#' }
#' @export
plot_jingle_jangle_scatter <- function(
    pair_table,
    thresholds = NULL,
    threshold_rule = c("wulff_mata", "quartile", "custom"),
    output_file = NULL,
    width = 7,
    height = 6,
    dpi = 300,
    point_alpha = .38,
    highlight_alpha = .65,
    point_size = 1.05,
    highlight_size = point_size,
    highlight_stroke = 0,
    region_alpha = 0,
    show_regression = TRUE,
    regression_colour = "#B23A7A",
    regression_linewidth = 1.1,
    palette = "wulff_mata",
    show_threshold_note = TRUE,
    min_constructs = 5,
    max_constructs = 100) {

  threshold_rule <- match.arg(threshold_rule)
  dat <- pair_table
  dat <- dat[is.finite(dat$scale_cos) & is.finite(dat$label_cos), , drop = FALSE]

  if (threshold_rule == "custom") {
    if (is.null(thresholds)) {
      stop("`thresholds` must be supplied when `threshold_rule = \"custom\"`.", call. = FALSE)
    }
  } else if (threshold_rule == "quartile") {
    thresholds <- c(
      scale_low = unname(stats::quantile(dat$scale_cos, .25, na.rm = TRUE)),
      label_low = unname(stats::quantile(dat$label_cos, .25, na.rm = TRUE)),
      scale_high = unname(stats::quantile(dat$scale_cos, .75, na.rm = TRUE)),
      label_high = unname(stats::quantile(dat$label_cos, .75, na.rm = TRUE))
    )
  } else if (is.null(thresholds)) {
    thresholds <- jingle_jangle_thresholds(
      scale_cos = dat$scale_cos,
      label_cos = dat$label_cos,
      min_constructs = min_constructs,
      max_constructs = max_constructs
    )
  }

  dat <- classify_jingle_jangle(dat, thresholds, verbose = FALSE)

  dat$type_plot <- ifelse(dat$type %in% c("Jingle fallacy", "Jangle fallacy"), dat$type, "Other")
  dat$type_plot <- factor(dat$type_plot, levels = c("Other", "Jingle fallacy", "Jangle fallacy"))

  cols <- embeddcv_relabel_palette(10, palette = palette, end = .9)
  plot_cols <- c(
    Other = "#8F9AA3",
    `Jingle fallacy` = cols[1],
    `Jangle fallacy` = cols[9]
  )
  plot_fills <- plot_cols

  xlim <- c(0, 1)
  ylim <- c(0, 1)

  rects <- data.frame(
    type_plot = factor(c("Jingle fallacy", "Jangle fallacy"),
                       levels = c("Other", "Jingle fallacy", "Jangle fallacy")),
    xmin = c(xlim[1], thresholds[["scale_high"]]),
    xmax = c(thresholds[["scale_low"]], xlim[2]),
    ymin = c(thresholds[["label_high"]], ylim[1]),
    ymax = c(ylim[2], thresholds[["label_low"]])
  )

  jingle_x <- xlim[1] + diff(xlim) * .035
  jingle_y <- ylim[2] - diff(ylim) * .035
  jangle_x <- xlim[2] - diff(xlim) * .035
  jangle_y <- ylim[1] + diff(ylim) * .035

  regression_layer <- if (isTRUE(show_regression)) {
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      colour = regression_colour,
      linewidth = regression_linewidth,
      linetype = 1,
      formula = y ~ x
    )
  } else {
    NULL
  }

  threshold_quantile_note <- if (threshold_rule == "wulff_mata") {
    paste0(
      "quantiles: low = q", round((1 - 1 / min_constructs) * 100),
      ", high = q", round((1 - 1 / max_constructs) * 100)
    )
  } else if (threshold_rule == "quartile") {
    "quantiles: low = q25, high = q75"
  } else {
    "quantiles: custom thresholds"
  }

  threshold_value_note <- paste0(
    "cutoffs: scale low = ", sprintf("%.3f", thresholds[["scale_low"]]),
    ", scale high = ", sprintf("%.3f", thresholds[["scale_high"]]),
    "; label low = ", sprintf("%.3f", thresholds[["label_low"]]),
    ", label high = ", sprintf("%.3f", thresholds[["label_high"]])
  )

  threshold_note <- if (isTRUE(show_threshold_note)) {
    paste(threshold_quantile_note, threshold_value_note, sep = "; ")
  } else {
    NULL
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$scale_cos, y = .data$label_cos)) +
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                   ymax = .data$ymax, fill = .data$type_plot),
      inherit.aes = FALSE,
      show.legend = FALSE,
      alpha = region_alpha,
      colour = NA
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        fill = .data$type_plot,
        alpha = .data$type_plot,
        size = .data$type_plot
      ),
      shape = 21,
      colour = "transparent",
      stroke = highlight_stroke
    ) +
    regression_layer +
    ggplot2::geom_vline(
      xintercept = c(thresholds[["scale_low"]], thresholds[["scale_high"]]),
      colour = "grey35",
      linewidth = .45,
      linetype = c("dashed", "solid")
    ) +
    ggplot2::geom_hline(
      yintercept = c(thresholds[["label_low"]], thresholds[["label_high"]]),
      colour = "grey35",
      linewidth = .45,
      linetype = c("dashed", "solid")
    ) +
    ggplot2::annotate(
      "label",
      x = jingle_x,
      y = jingle_y,
      label = "Jingle Fallacies",
      hjust = 0,
      vjust = 1,
      size = 3.4,
      fontface = "bold",
      linewidth = .22,
      label.r = grid::unit(.08, "lines"),
      colour = plot_cols[["Jingle fallacy"]],
      fill = "white"
    ) +
    ggplot2::annotate(
      "label",
      x = jangle_x,
      y = jangle_y,
      label = "Jangle Fallacies",
      hjust = 1,
      vjust = 0,
      size = 3.4,
      fontface = "bold",
      linewidth = .22,
      label.r = grid::unit(.08, "lines"),
      colour = plot_cols[["Jangle fallacy"]],
      fill = "white"
    ) +
    ggplot2::scale_fill_manual(values = plot_fills, name = NULL) +
    ggplot2::scale_alpha_manual(
      values = c(
        Other = point_alpha,
        `Jingle fallacy` = highlight_alpha,
        `Jangle fallacy` = highlight_alpha
      ),
      guide = "none"
    ) +
    ggplot2::scale_size_manual(
      values = c(
        Other = point_size,
        `Jingle fallacy` = highlight_size,
        `Jangle fallacy` = highlight_size
      ),
      guide = "none"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(alpha = 1, size = 2.6, shape = 21, colour = "transparent")
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, .1),
      expand = ggplot2::expansion(mult = c(.015, .015))
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, .1),
      expand = ggplot2::expansion(mult = c(.015, .015))
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(
      x = "Scale similarity",
      y = "Label similarity",
      title = "Jingle and jangle fallacy regions",
      subtitle = "Each point is a pair of scales; guides mark semantic similarity thresholds",
      caption = threshold_note
    ) +
    ggplot2::theme_light(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, colour = "grey35")
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}

#' Plot a jingle-jangle line figure
#'
#' Draws the line figure used to visualize jingle and jangle fallacies in
#' Wulff & Mata (2025). Each line is one pair of scales: the left endpoint is
#' label similarity and the right endpoint is scale similarity. Grey lines are
#' ordinary pairs that do not meet either fallacy criterion; colored lines are
#' pairs classified as jingle or jangle fallacies. Sampled fallacy labels are
#' printed on the left and right sides.
#'
#' @details
#' Each pair is drawn as a line connecting label similarity on the left axis to
#' scale similarity on the right axis. Jingle pairs have high label similarity
#' but low scale similarity; jangle pairs have low label similarity but high
#' scale similarity. The thresholds determine the bracket positions and should
#' usually be the same thresholds used by `classify_jingle_jangle()`.
#'
#' If `output_file` is supplied, a PNG device is opened with
#' `grDevices::png(width = width, height = height, res = res, units = "in")`.
#' When `auto_height = TRUE`, the saved PNG height is increased as needed so
#' sampled side labels can keep a constant text size instead of being compressed
#' into the original plotting height.
#'
#' @param pair_table Data frame or tibble returned by
#'   `classify_jingle_jangle()`. It must contain numeric `label_cos`,
#'   `scale_cos`, and `cos_diff` columns, a character/factor `type` column with
#'   values `"Jingle fallacy"`, `"Jangle fallacy"`, and `"nothing"`, and a
#'   character `label` column used for side labels.
#' @param thresholds Named numeric vector returned by
#'   `jingle_jangle_thresholds()`, with `scale_low`, `label_low`,
#'   `scale_high`, and `label_high`. The values determine the bracket positions
#'   and should match the thresholds used to classify `pair_table`.
#' @param output_file Optional character path. If supplied, a PNG is written
#'   with `grDevices::png()`; if `NULL`, the plot is drawn on the active device.
#' @param width,height Numeric scalars. PNG size in inches when `output_file` is
#'   supplied.
#' @param res Numeric scalar. PNG resolution in pixels per inch when
#'   `output_file` is supplied.
#' @param n_labels Integer scalar. Maximum number of highlighted fallacy labels
#'   sampled and printed per side. If more fallacies exist, all fallacy line
#'   segments are still drawn, but only `n_labels` side labels are sampled. Use
#'   `Inf` to print all highlighted labels. When `output_file` is supplied and
#'   `auto_height = TRUE`, the figure height grows with the number of printed
#'   labels to reduce overlap.
#' @param n_background Integer scalar. Maximum number of grey background lines
#'   to draw. Lower values make the plot faster and lighter; higher values make
#'   the background density closer to the full pair table.
#' @param seed Integer scalar. Random seed for reproducible fallacy label and
#'   background sampling.
#' @param background_col Character scalar. Color for non-fallacy background
#'   lines.
#' @param background_lwd Numeric scalar. Line width for non-fallacy background
#'   lines.
#' @param fallacy_lwd Numeric scalar. Line width for jingle and jangle fallacy
#'   lines.
#' @param label_cex Numeric scalar. Constant text size for side labels.
#' @param auto_height Logical scalar. If `TRUE` and `output_file` is supplied,
#'   increases `height` when many side labels are printed.
#' @param label_line_height Numeric scalar. Approximate inches reserved per
#'   printed side label when `auto_height = TRUE`.
#' @param palette Character scalar. Color palette for jingle and jangle lines
#'   and labels. Options
#'   are `"wulff_mata"` (default), `"cividis"`, `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#' @param show_count_note Logical scalar. If `TRUE`, adds a footnote below the
#'   figure with the number of jingle fallacies, jangle fallacies, and their
#'   total. The default is `FALSE` so package users opt in explicitly.
#' @param count_note Optional character scalar. Custom note to draw when
#'   `show_count_note = TRUE`. If `NULL`, the note is generated from
#'   `pair_table` and starts with an asterisk.
#' @param count_note_cex Numeric scalar. Text size for the fallacy count note.
#' @param count_note_col Character scalar. Text color for the fallacy count
#'   note.
#' @param verbose Logical scalar. If `TRUE`, prints the number of jingle and
#'   jangle fallacies in `pair_table` and the number of side labels drawn.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @return Invisibly returns `pair_table`.
#' @export
plot_jingle_jangle <- function(
    pair_table,
    thresholds,
    output_file = NULL,
    width = 8.7,
    height = 6.5,
    res = 600,
    n_labels = 60,
    n_background = 10000,
    seed = 42,
    background_col = "grey85",
    background_lwd = .07,
    fallacy_lwd = .07,
    label_cex = .5,
    auto_height = TRUE,
    label_line_height = .07,
    palette = "wulff_mata",
    show_count_note = FALSE,
    count_note = NULL,
    count_note_cex = .72,
    count_note_col = "grey35",
    verbose = TRUE) {

  cols <- embeddcv_relabel_palette(10, palette = palette, end = .9)[c(7, 1)]
  cols_bg <- background_col
  cols_txt <- embeddcv_relabel_palette(10, palette = palette, end = .9)[c(7, 1)]

  if (!"type" %in% names(pair_table) ||
      !"cos_diff" %in% names(pair_table) ||
      !"label" %in% names(pair_table)) {
    pair_table <- classify_jingle_jangle(pair_table, thresholds, verbose = verbose)
  }
  pair_table$type <- as.character(pair_table$type)
  pair_table$type[pair_table$type == "Good"] <- "nothing"

  jingle <- pair_table[pair_table$type == "Jingle fallacy", , drop = FALSE]
  jangle <- pair_table[pair_table$type == "Jangle fallacy", , drop = FALSE]
  n_jingle <- nrow(jingle)
  n_jangle <- nrow(jangle)
  n_fallacy_total <- n_jingle + n_jangle
  max_side_labels <- max(nrow(jingle), nrow(jangle), 1L)
  n_labels_plot <- if (is.infinite(n_labels)) max_side_labels else min(max_side_labels, n_labels)
  n_labels_plot <- max(1L, as.integer(n_labels_plot))
  n_label_slots <- if (is.infinite(n_labels)) n_labels_plot else max(1L, as.integer(n_labels))

  if (!is.null(output_file) && isTRUE(auto_height)) {
    height <- max(height, 1.7 + n_labels_plot * label_line_height)
  }

  if (isTRUE(verbose)) {
    embeddcv_message_jj_counts(pair_table, prefix = "Jingle-jangle plot input")
    message(
      "Jingle-jangle plot labels: drawing up to ", n_labels_plot,
      " labels per side (", nrow(jingle), " jingle fallacies; ",
      nrow(jangle), " jangle fallacies)."
    )
  }

  if (!is.null(output_file)) {
    grDevices::png(output_file, width = width, height = height, units = "in", res = res)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    if (!is.null(output_file)) grDevices::dev.off()
  }, add = TRUE)

  graphics::par(
    mar = c(0, 0, 1, 0),
    oma = c(if (isTRUE(show_count_note)) 2.1 else 0, 0, 0, 0)
  )
  graphics::plot.new()
  graphics::plot.window(ylim = c(0, 1), xlim = c(-3, 6))

  graphics::text(.45, seq(0, 1, .1), labels = seq(0, 1, .1), cex = .5, adj = 1)
  graphics::text(2.55, seq(0, 1, .1), labels = seq(0, 1, .1), cex = .5, adj = 0)
  graphics::text(0.1, .35, labels = "Label similarity", cex = .8, adj = .5, srt = 90)
  graphics::text(2.9, .35, labels = "Scale similarity", cex = .8, adj = .5, srt = 270)

  graphics::mtext("Jingle fallacy", at = -.5, side = 3, cex = 1, font = 2, adj = 1, line = -.5)
  graphics::mtext("Jangle fallacy", at = 3.5, side = 3, cex = 1, font = 2, adj = 0, line = -.5)
  graphics::mtext("Similar labels, dissimilar scales", at = -.5, side = 3, cex = .6, line = -1.3, adj = 1)
  graphics::mtext("Dissimilar labels, similar scales", at = 3.5, side = 3, cex = .6, line = -1.3, adj = 0)

  ypos <- seq(.03, .97, length = n_label_slots)
  nothing <- pair_table[pair_table$type == "nothing", , drop = FALSE]
  if (nrow(nothing) > 0) {
    set.seed(seed)
    nothing <- nothing[sample(seq_len(nrow(nothing)), min(nrow(nothing), n_background)), , drop = FALSE]
    for (i in seq_len(nrow(nothing))) {
      graphics::lines(
        y = c(nothing$label_cos[i], nothing$scale_cos[i]),
        x = c(.5, 2.5),
        col = cols_bg,
        lwd = background_lwd
      )
    }
  }

  embeddcv_plot_jj_side(jingle, side = "left", ypos = ypos, n_labels = n_labels_plot,
                        seed = seed, col = cols[2], txt_col = cols_txt[2],
                        fallacy_lwd = fallacy_lwd, label_cex = label_cex)

  graphics::lines(c(.1, .1), c(thresholds[["label_high"]], 1), lwd = .5)
  graphics::lines(c(.2, .1), c(thresholds[["label_high"]], thresholds[["label_high"]]), lwd = .5)
  graphics::lines(c(.2, .1), c(1, 1), lwd = .5)
  graphics::lines(c(-.3, .1), c(thresholds[["label_high"]], thresholds[["label_high"]]), lwd = .5)

  embeddcv_plot_jj_side(jangle, side = "right", ypos = ypos, n_labels = n_labels_plot,
                        seed = seed, col = cols[1], txt_col = cols_txt[1],
                        fallacy_lwd = fallacy_lwd, label_cex = label_cex)

  graphics::lines(c(2.9, 2.9), c(thresholds[["scale_high"]], .97), lwd = .5)
  graphics::lines(c(2.8, 2.9), c(thresholds[["scale_high"]], thresholds[["scale_high"]]), lwd = .5)
  graphics::lines(c(2.8, 2.9), c(.97, .97), lwd = .5)
  graphics::lines(c(2.9, 3.3), c(thresholds[["scale_high"]], thresholds[["scale_high"]]), lwd = .5)

  if (isTRUE(show_count_note)) {
    if (is.null(count_note)) {
      count_note <- paste0(
        "* Jingle fallacies = ", n_jingle,
        "; jangle fallacies = ", n_jangle,
        "; total fallacies = ", n_fallacy_total,
        "."
      )
    }
    graphics::mtext(
      count_note,
      side = 1,
      outer = TRUE,
      line = .55,
      adj = .02,
      cex = count_note_cex,
      col = count_note_col
    )
  }

  invisible(pair_table)
}

embeddcv_plot_jj_side <- function(dat, side, ypos, n_labels, seed, col, txt_col, fallacy_lwd, label_cex) {
  if (nrow(dat) == 0) return(invisible(NULL))

  for (i in seq_len(nrow(dat))) {
    graphics::lines(
      y = c(dat$label_cos[i], dat$scale_cos[i]),
      x = c(.5, 2.5),
      col = col,
      lwd = fallacy_lwd
    )
  }

  set.seed(seed)
  sel <- dat[sample(seq_len(nrow(dat)), min(nrow(dat), n_labels)), , drop = FALSE]
  sel <- sel[order(sel$cos_diff), , drop = FALSE]
  ys <- ypos[(1 + length(ypos) - nrow(sel)):length(ypos)]

  if (identical(side, "left")) {
    graphics::text(
      -.5,
      ys,
      labels = sel$label,
      adj = 1,
      cex = label_cex,
      col = txt_col
    )
    graphics::lines(c(-.3, -.3), c(.03, .97), lwd = .5)
    graphics::lines(c(-.4, -.3), c(.97, .97), lwd = .5)
    graphics::lines(c(-.4, -.3), c(.03, .03), lwd = .5)
  } else {
    graphics::text(3.5, ys, labels = sel$label, adj = 0, cex = label_cex, col = txt_col)
    graphics::lines(c(3.3, 3.3), c(min(ys), .97), lwd = .5)
    graphics::lines(c(3.4, 3.3), c(.97, .97), lwd = .5)
    graphics::lines(c(3.4, 3.3), c(min(ys), min(ys)), lwd = .5)
  }

  invisible(NULL)
}

embeddcv_message_jj_counts <- function(dat, prefix = "Jingle-jangle") {
  type <- as.character(dat$type)
  n_jingle <- sum(type == "Jingle fallacy", na.rm = TRUE)
  n_jangle <- sum(type == "Jangle fallacy", na.rm = TRUE)
  n_ok <- sum(type %in% c("nothing", "Good"), na.rm = TRUE)
  message(
    prefix, ": ",
    n_jingle, " jingle fallacies; ",
    n_jangle, " jangle fallacies; ",
    n_ok, " non-fallacy pairs."
  )
  invisible(c(jingle = n_jingle, jangle = n_jangle, ok = n_ok))
}
