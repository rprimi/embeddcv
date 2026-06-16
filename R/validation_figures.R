#' Semantic validity indices for one instrument
#'
#' Computes, for a single instrument, the main semantic validity indices for
#' each scale from item embeddings: a semantic reliability (alpha), structural
#' fidelity, and — when a construct grouping is supplied — convergent validity.
#' All indices share the same logic: compare a scale's *within-scale* similarity
#' against its similarity to *other scales*. Multiple instruments are handled in
#' the analysis pipeline by calling this function once per instrument.
#'
#' @details
#' Item similarities are aggregated to a scale-by-scale matrix \eqn{S} with
#' \code{\link{get_item_scale_cos}} (the within-scale value \eqn{S_{jj}} is the
#' mean item-item similarity inside scale \eqn{j}, excluding self-pairs). Then:
#' \describe{
#'   \item{\code{alpha_semantic}}{Spearman-Brown prediction
#'     \eqn{n \bar{s} / (1 + (n-1)\bar{s})}, with \eqn{\bar{s} = S_{jj}} and
#'     \eqn{n} the number of items.}
#'   \item{\code{fidelity}}{z-score of \eqn{S_{jj}} within the scale's row across
#'     all scales (how far above its mean the within-scale similarity sits).}
#'   \item{\code{convergent_similarity}}{(only with \code{construct_by_scale})
#'     mean z-scored similarity of the scale to the *other* scales of the same
#'     construct.}
#' }
#'
#' @param item_emb Numeric matrix/data frame of item embeddings (rows = items),
#'   or a precomputed item-item cosine matrix (a square matrix whose row and
#'   column names are identical). Row names are item ids.
#' @param scale_id Character/factor vector, one scale per item. Either aligned
#'   to the rows of \code{item_emb} (same length/order) or named by item id.
#' @param construct_by_scale Optional. Character vector mapping each scale to a
#'   construct, named by scale id or aligned to the unique scales. When given,
#'   \code{convergent_similarity} is added.
#' @param min_scales Integer. Minimum number of scales required for
#'   \code{fidelity} (and minimum construct size for \code{convergent_similarity}).
#'   Default 3.
#' @param self_cutoff Numeric. Item self-similarities at or above this value are
#'   excluded when aggregating within-scale similarity. Default \code{.999999}.
#'
#' @return A data frame, one row per scale: \code{scale}, \code{n_items},
#'   \code{mean_item_similarity}, \code{alpha_semantic}, \code{fidelity},
#'   \code{n_scales}, and (when \code{construct_by_scale} is supplied)
#'   \code{construct}, \code{convergent_similarity}.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954. https://doi.org/10.1038/s41562-024-02089-y
#'
#' @seealso \code{\link{plot_semantic_validity}}, \code{\link{divergent_validity}}
#'
#' @examples
#' \dontrun{
#' v <- semantic_validity(item_emb, scale_id = item_dic$scale,
#'                        construct_by_scale = construct_by_scale)
#' head(v)
#' plot_semantic_validity(v, index = "fidelity")
#' }
#'
#' @export
semantic_validity <- function(
    item_emb,
    scale_id,
    construct_by_scale = NULL,
    min_scales = 3,
    self_cutoff = .999999) {

  m <- as.matrix(item_emb)

  # Treat as a precomputed item cosine matrix only when square with identical
  # row/column names; otherwise compute the item-item cosine matrix.
  item_cos <- if (!is.null(rownames(m)) && !is.null(colnames(m)) &&
                  nrow(m) == ncol(m) && identical(rownames(m), colnames(m))) {
    m
  } else {
    cosine_matrix(m)
  }
  item_ids <- rownames(item_cos)
  if (is.null(item_ids)) {
    stop("`item_emb` must have row names (item ids).", call. = FALSE)
  }

  scale_vec <- if (!is.null(names(scale_id))) {
    as.character(scale_id[item_ids])
  } else {
    as.character(scale_id)
  }
  if (length(scale_vec) != length(item_ids)) {
    stop("`scale_id` must have one entry per item (length nrow(item_emb)) ",
         "or be named by item id.", call. = FALSE)
  }

  scale_cos <- get_item_scale_cos(item_cos, scale_vec, self_cutoff = self_cutoff)
  scales <- rownames(scale_cos)
  n_items_tbl <- table(scale_vec)
  n_scales <- length(scales)

  construct <- NULL
  construct_sizes <- NULL
  if (!is.null(construct_by_scale)) {
    if (!is.null(names(construct_by_scale))) {
      construct <- as.character(construct_by_scale[scales])
    } else {
      if (length(construct_by_scale) != n_scales) {
        stop("`construct_by_scale` must be named by scale id or have length = ",
             "number of scales (", n_scales, ").", call. = FALSE)
      }
      construct <- as.character(construct_by_scale)
    }
    names(construct) <- scales
    construct_sizes <- table(construct)
  }

  rows <- lapply(scales, function(s) {
    n <- as.integer(n_items_tbl[[s]])
    s_bar <- unname(scale_cos[s, s])
    alpha <- if (is.finite(s_bar) && n > 1) n * s_bar / (1 + (n - 1) * s_bar) else NA_real_
    z <- embeddcv_z(scale_cos[s, ])
    fidelity <- if (n_scales >= min_scales) unname(z[s]) else NA_real_

    row <- data.frame(
      scale = s,
      n_items = n,
      mean_item_similarity = s_bar,
      alpha_semantic = alpha,
      fidelity = fidelity,
      n_scales = n_scales,
      stringsAsFactors = FALSE
    )
    if (!is.null(construct)) {
      cons <- construct[[s]]
      same <- names(construct)[construct == cons & names(construct) != s]
      conv <- if (!is.na(cons) && length(same) > 0 &&
                  isTRUE(construct_sizes[[cons]] >= min_scales)) {
        mean(z[same], na.rm = TRUE)
      } else {
        NA_real_
      }
      row$construct <- cons
      row$convergent_similarity <- conv
    }
    row
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Plot a semantic validity index by scale
#'
#' Bar (or point) plot of one per-scale index from \code{\link{semantic_validity}},
#' ordered from highest to lowest. A dashed reference line marks the recovery
#' criterion for z-scored indices.
#'
#' @param tbl Data frame from \code{semantic_validity()}.
#' @param index Character. Which column to plot. One of \code{"fidelity"}
#'   (default), \code{"alpha_semantic"}, \code{"convergent_similarity"},
#'   \code{"mean_item_similarity"}.
#' @param geom Character. \code{"bar"} (default) or \code{"point"}.
#' @param recovery_line Numeric or \code{NULL}. Horizontal reference line.
#'   Defaults to \code{2} for the z-scored indices (\code{fidelity},
#'   \code{convergent_similarity}) and \code{NULL} otherwise.
#' @param show_labels Logical. Show scale labels on the x axis (default
#'   \code{TRUE}).
#' @param label_fun Function mapping scale ids to display labels.
#' @param palette Character. Color palette (see \code{plot_divergent_validity}).
#' @param output_file,width,height,dpi Optional save path and size for
#'   \code{ggplot2::ggsave()}.
#'
#' @return A ggplot object (also saved when \code{output_file} is supplied).
#'
#' @seealso \code{\link{semantic_validity}}
#' @export
plot_semantic_validity <- function(
    tbl,
    index = c("fidelity", "alpha_semantic", "convergent_similarity", "mean_item_similarity"),
    geom = c("bar", "point"),
    recovery_line = NULL,
    show_labels = TRUE,
    label_fun = identity,
    palette = "wulff_mata",
    output_file = NULL,
    width = 9,
    height = 4.8,
    dpi = 300) {

  index <- match.arg(index)
  geom <- match.arg(geom)
  if (!index %in% names(tbl)) {
    stop("`tbl` has no column `", index, "`. Available: ",
         paste(names(tbl), collapse = ", "), call. = FALSE)
  }

  d <- tbl[is.finite(tbl[[index]]), , drop = FALSE]
  if (nrow(d) == 0) stop("No finite values of `", index, "` to plot.", call. = FALSE)
  d <- d[order(d[[index]], decreasing = TRUE), , drop = FALSE]
  d$scale <- factor(d$scale, levels = d$scale)

  if (is.null(recovery_line) && index %in% c("fidelity", "convergent_similarity")) {
    recovery_line <- 2
  }
  fill <- embeddcv_relabel_palette(1, palette = palette, end = .6)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$scale, y = .data[[index]]))
  p <- p + if (identical(geom, "bar")) {
    ggplot2::geom_col(fill = fill, width = .75)
  } else {
    ggplot2::geom_point(colour = fill, size = 2.4)
  }
  if (!is.null(recovery_line)) {
    p <- p + ggplot2::geom_hline(yintercept = recovery_line,
                                 linetype = "dashed", colour = "#D8B365")
  }
  p <- p +
    ggplot2::labs(x = NULL, y = index) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (isTRUE(show_labels)) {
    p <- p +
      ggplot2::scale_x_discrete(labels = function(x) label_fun(x)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1,
                                                         vjust = .5, size = 7))
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }
  p
}


#' Divergent validity: predicted vs. empirical correlations
#'
#' Correlates semantic similarities with *external* empirical correlations for a
#' set of pairs. This is a different kind of validation from
#' \code{\link{semantic_validity}} because it needs an external criterion
#' (observed correlations), so it is kept separate. Operates on a single
#' similarity matrix; compare instruments/models in the pipeline.
#'
#' @param similarity Numeric similarity matrix with row/column names (item or
#'   scale ids).
#' @param empirical_pairs Data frame of pairs with id columns and an observed
#'   value column.
#' @param id_1_col,id_2_col Names of the two id columns in \code{empirical_pairs}.
#' @param observed_col Name of the observed value column (e.g. absolute
#'   correlation). Default \code{"abs_cor"}.
#' @param group_col Optional grouping column (e.g. inventory). If \code{NULL},
#'   all pairs form one group.
#' @param similarity_type Character label stored in the \code{type} column.
#'
#' @return A data frame with \code{group}, \code{type}, \code{n_pairs},
#'   \code{correlation} (Pearson r of predicted vs. observed), and \code{mae}.
#'
#' @seealso \code{\link{plot_divergent_validity}}
#' @export
divergent_validity <- function(
    similarity,
    empirical_pairs,
    id_1_col,
    id_2_col,
    observed_col = "abs_cor",
    group_col = NULL,
    similarity_type = "Similarity") {

  if (!is.data.frame(empirical_pairs)) {
    stop("`empirical_pairs` must be a data frame.", call. = FALSE)
  }
  required <- c(id_1_col, id_2_col, observed_col)
  if (!is.null(group_col)) required <- c(required, group_col)
  missing <- setdiff(required, names(empirical_pairs))
  if (length(missing) > 0) {
    stop("`empirical_pairs` is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  mat <- as.matrix(similarity)
  if (is.null(rownames(mat)) || is.null(colnames(mat))) {
    stop("`similarity` must have row and column names.", call. = FALSE)
  }

  groups <- if (is.null(group_col)) {
    rep("All pairs", nrow(empirical_pairs))
  } else {
    as.character(empirical_pairs[[group_col]])
  }
  group_levels <- unique(groups)

  ids_1 <- as.character(empirical_pairs[[id_1_col]])
  ids_2 <- as.character(empirical_pairs[[id_2_col]])
  available <- ids_1 %in% rownames(mat) & ids_2 %in% colnames(mat)
  pred <- rep(NA_real_, nrow(empirical_pairs))
  pred[available] <- mat[cbind(ids_1[available], ids_2[available])]
  obs <- as.numeric(empirical_pairs[[observed_col]])

  rows <- lapply(group_levels, function(g) {
    idx <- groups == g & is.finite(pred) & is.finite(obs)
    data.frame(
      group = g,
      type = similarity_type,
      n_pairs = sum(idx),
      correlation = if (sum(idx) >= 3) stats::cor(pred[idx], obs[idx]) else NA_real_,
      mae = if (sum(idx) > 0) mean(abs(pred[idx] - obs[idx])) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Plot divergent-validity recovery
#'
#' Bar plot of the predicted-vs-empirical correlations from
#' \code{\link{divergent_validity}}, by group and similarity type.
#'
#' @param divergent_tbl Data frame from \code{divergent_validity()} (or several
#'   \code{rbind()}-ed together). Must contain \code{group}, \code{type},
#'   \code{correlation}.
#' @param palette Character. Color palette for \code{type}.
#' @param y_limits Numeric length-2 y-axis limits.
#' @param output_file,width,height,dpi Optional save path and size.
#'
#' @return A ggplot object (also saved when \code{output_file} is supplied).
#' @seealso \code{\link{divergent_validity}}
#' @export
plot_divergent_validity <- function(
    divergent_tbl,
    output_file = NULL,
    width = 7,
    height = 4,
    dpi = 600,
    palette = "wulff_mata",
    y_limits = c(-.3, 1)) {

  if (!all(c("group", "type", "correlation") %in% names(divergent_tbl))) {
    stop("`divergent_tbl` must contain `group`, `type`, and `correlation`.", call. = FALSE)
  }
  dat <- divergent_tbl[is.finite(divergent_tbl$correlation), , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("`divergent_tbl` has no finite correlations to plot.", call. = FALSE)
  }

  dat$group <- factor(dat$group, levels = unique(dat$group))
  dat$type <- factor(dat$type, levels = unique(dat$type))
  type_levels <- levels(dat$type)
  cols <- embeddcv_relabel_palette(length(type_levels), palette = palette, begin = .1, end = .9)
  names(cols) <- type_levels

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$group, y = .data$correlation,
                                         fill = .data$type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = .8), width = .7) +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(x = NULL, y = "Correlation (predicted vs. empirical)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }
  p
}


#' Plot a cosine similarity distribution
#'
#' Draws a polished histogram with density overlay for a vector, matrix, or data
#' frame of cosine similarities, with optional quartile/percentile markers.
#' Useful for inspecting item-item, scale-scale, definition-definition, or
#' cross-set similarities before choosing thresholds.
#'
#' @param similarities Numeric vector, similarity matrix, or data frame
#'   (with a similarity column).
#' @param value_col Column name when \code{similarities} is a data frame. If
#'   \code{NULL}, the first of \code{cosim}/\code{similarity}/\code{value}/
#'   \code{cosine}/\code{cos} present is used.
#' @param matrix_triangle For square matrices, which values to use:
#'   \code{"upper"} (default), \code{"lower"}, or \code{"all"}.
#' @param bins Integer histogram bins.
#' @param show_quartiles,show_percentiles,percentiles,all_deciles Marker controls.
#' @param title,subtitle,x_label,y_label Plot labels.
#' @param palette,hist_fill,density_col,marker_col Colors.
#' @param line_label_digits Digits for marker value labels.
#' @param output_file,width,height,dpi Optional save path and size.
#'
#' @return A ggplot object (also saved when \code{output_file} is supplied).
#' @export
plot_similarity_distribution <- function(
    similarities,
    value_col = NULL,
    output_file = NULL,
    width = 7.5,
    height = 4.8,
    dpi = 600,
    bins = 42,
    palette = "wulff_mata",
    hist_fill = "#2B6CB0",
    density_col = "#E66101",
    marker_col = "#B2182B",
    title = "Cosine similarity distribution",
    subtitle = NULL,
    x_label = "Cosine similarity",
    y_label = "Number of pairs",
    show_quartiles = TRUE,
    show_percentiles = FALSE,
    percentiles = c(.10, .30, .50, .70, .90),
    all_deciles = FALSE,
    matrix_triangle = c("upper", "lower", "all"),
    line_label_digits = 2) {

  matrix_triangle <- match.arg(matrix_triangle)
  values <- embeddcv_similarity_values(similarities, value_col, matrix_triangle)
  if (length(values) == 0) {
    stop("No finite similarity values were found.", call. = FALSE)
  }

  if (is.null(subtitle)) {
    subtitle <- sprintf(
      "N = %s pairs; mean = %.3f; SD = %.3f",
      format(length(values), big.mark = ",", scientific = FALSE),
      mean(values),
      stats::sd(values)
    )
  }

  hist_edge <- "white"

  marker_tbl <- embeddcv_similarity_marker_table(
    values = values,
    show_quartiles = show_quartiles,
    show_percentiles = show_percentiles,
    percentiles = percentiles,
    all_deciles = all_deciles,
    digits = line_label_digits
  )

  hist_obj <- graphics::hist(values, breaks = bins, plot = FALSE)
  max_count <- max(hist_obj$counts, na.rm = TRUE)
  value_range <- range(values, na.rm = TRUE)
  value_span <- diff(value_range)
  if (!is.finite(value_span) || value_span == 0) {
    value_span <- .01
  }
  x_limits <- value_range + c(-.02, .02) * value_span
  binwidth <- diff(range(hist_obj$breaks)) / length(hist_obj$counts)
  n_values <- length(values)

  marker_colours <- c(Quartile = marker_col, Percentile = marker_col)
  marker_label_y <- -max_count * .075
  marker_value_y <- -max_count * .145
  y_breaks <- scales::pretty_breaks(n = 5)(c(0, max_count * 1.28))
  y_breaks <- y_breaks[y_breaks >= 0]

  dat <- data.frame(value = values)
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(
      bins = bins,
      fill = hist_fill,
      colour = hist_edge,
      linewidth = .18
    ) +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(.data$density * n_values * binwidth)),
      linewidth = 1,
      colour = density_col,
      alpha = .9
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = scales::label_number()
    ) +
    ggplot2::coord_cartesian(
      xlim = x_limits,
      ylim = c(-max_count * .19, max_count * 1.28),
      clip = "off"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey88", linewidth = .25),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = .25),
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      plot.margin = ggplot2::margin(14, 10, 24, 8)
    )

  if (nrow(marker_tbl) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = marker_tbl,
        ggplot2::aes(
          x = .data$value,
          xend = .data$value,
          y = 0,
          yend = max_count * 1.12,
          colour = .data$kind
        ),
        inherit.aes = FALSE,
        linetype = 2,
        linewidth = .45
      ) +
      ggplot2::geom_text(
        data = marker_tbl,
        ggplot2::aes(x = .data$value, y = marker_label_y, label = .data$label, colour = .data$kind),
        inherit.aes = FALSE,
        fontface = "bold",
        size = 3.2
      ) +
      ggplot2::geom_text(
        data = marker_tbl,
        ggplot2::aes(x = .data$value, y = marker_value_y, label = .data$value_label),
        inherit.aes = FALSE,
        colour = "black",
        size = 2.7
      ) +
      ggplot2::scale_colour_manual(values = marker_colours, guide = "none")
  }

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}


# ── Internal helpers ─────────────────────────────────────────────────────────

embeddcv_similarity_values <- function(similarities, value_col = NULL, matrix_triangle = "upper") {
  if (is.data.frame(similarities)) {
    if (is.null(value_col)) {
      candidate_cols <- c("cosim", "similarity", "value", "cosine", "cos")
      value_col <- candidate_cols[candidate_cols %in% names(similarities)][1]
      if (is.na(value_col)) {
        stop(
          "`value_col` is required when `similarities` is a data frame without ",
          "a `cosim`, `similarity`, `value`, `cosine`, or `cos` column.",
          call. = FALSE
        )
      }
    }
    if (!value_col %in% names(similarities)) {
      stop("`value_col` is not a column in `similarities`.", call. = FALSE)
    }
    values <- similarities[[value_col]]
  } else if (is.matrix(similarities) || length(dim(similarities)) == 2) {
    mat <- as.matrix(similarities)
    if (nrow(mat) == ncol(mat) && matrix_triangle != "all") {
      keep <- switch(
        matrix_triangle,
        upper = upper.tri(mat, diag = FALSE),
        lower = lower.tri(mat, diag = FALSE)
      )
      values <- mat[keep]
    } else {
      values <- as.vector(mat)
    }
  } else {
    values <- as.vector(similarities)
  }

  values <- as.numeric(values)
  values[is.finite(values)]
}

embeddcv_similarity_marker_table <- function(
    values,
    show_quartiles,
    show_percentiles,
    percentiles,
    all_deciles,
    digits) {

  rows <- list()
  if (isTRUE(show_quartiles)) {
    q_probs <- c(.25, .50, .75)
    q_values <- stats::quantile(values, probs = q_probs, names = FALSE, na.rm = TRUE)
    rows[[length(rows) + 1L]] <- data.frame(
      kind = "Quartile",
      prob = q_probs,
      label = paste0("Q", round(q_probs * 100)),
      value = q_values,
      stringsAsFactors = FALSE
    )
  }

  if (isTRUE(show_percentiles)) {
    p_probs <- if (isTRUE(all_deciles)) seq(.10, .90, .10) else percentiles
    if (any(p_probs > 1, na.rm = TRUE)) {
      p_probs <- p_probs / 100
    }
    p_probs <- p_probs[is.finite(p_probs) & p_probs > 0 & p_probs < 1]
    if (length(p_probs) > 0) {
      p_values <- stats::quantile(values, probs = p_probs, names = FALSE, na.rm = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        kind = "Percentile",
        prob = p_probs,
        label = paste0("P", round(p_probs * 100)),
        value = p_values,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(kind = character(), prob = numeric(), label = character(),
                      value = numeric(), value_label = character()))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$value, out$prob, out$kind), , drop = FALSE]
  out$value_label <- formatC(out$value, format = "f", digits = digits)
  rownames(out) <- NULL
  out
}

embeddcv_z <- function(x) {
  nm <- names(x)
  x <- as.numeric(x)
  mu <- mean(x, na.rm = TRUE)
  sig <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sig) || sig == 0) {
    out <- rep(NA_real_, length(x))
    names(out) <- nm
    return(out)
  }
  out <- (x - mu) / sig
  names(out) <- nm
  out
}
