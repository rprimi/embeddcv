#' Compute structural fidelity of scale item sets
#'
#' Computes the structural fidelity score used by Wulff & Mata (2025) to test
#' whether items from the same scale are semantically closer to one another
#' than to items from other scales in the same inventory. The function accepts
#' one item-item similarity matrix or a named list of matrices, making it
#' possible to compare multiple embedding models in the same plot.
#'
#' @details
#' The input item similarity matrix is first aggregated to a scale-by-scale
#' matrix. For focal scale \eqn{j}, comparison scale \eqn{r}, and inventory
#' \eqn{k}, let
#'
#' \deqn{S_{jrk} = \operatorname{mean}\{C_{ab}: a \in I_j,\ b \in I_r\},}
#'
#' where \eqn{C_{ab}} is the item-item cosine similarity and \eqn{I_j} is the
#' set of items in scale \eqn{j}. For \eqn{j = r}, item self-similarities are
#' excluded before averaging.
#'
#' Structural fidelity is then the z-score of the within-scale value
#' \eqn{S_{jjk}} relative to the focal scale's similarities to all scales in the
#' same inventory:
#'
#' \deqn{
#' \operatorname{fidelity}_{jk} =
#' \frac{S_{jjk} - N_k^{-1}\sum_{r=1}^{N_k} S_{jrk}}
#' {\sqrt{\frac{1}{N_k - 1}\sum_{r=1}^{N_k}
#' \left(S_{jrk} - N_k^{-1}\sum_{q=1}^{N_k} S_{jqk}\right)^2}},
#' }
#'
#' where \eqn{N_k} is the number of scales in inventory \eqn{k}. This is the
#' same row-wise z-score computed in Wulff & Mata (2025); the extra comparison
#' index \eqn{r} is written explicitly here because the function stores the
#' scale-by-scale similarities in a matrix. Wulff & Mata (2025) interpreted
#' \eqn{z > 2} as full recovery and \eqn{z > 1} as partial recovery.
#'
#' @param item_cos Numeric square item-item similarity matrix, or a named list
#'   of such matrices. Rows and columns must represent the same items. Matrices
#'   are usually produced by `cosine_matrix(item_embeddings)`.
#' @param scale_id Character vector assigning each item to a scale. If
#'   `names(scale_id)` are present, they are matched to the row names of each
#'   matrix in `item_cos`; otherwise the vector must be in the same order as the
#'   matrix rows.
#' @param inventory Optional character vector assigning each item to an
#'   inventory or instrument. If `NULL`, `instrument_from_scale(scale_id)` is
#'   used. If supplied with names, names are matched to item ids.
#' @param model_names Optional character vector used to name the similarity
#'   matrices. When `item_cos` is a named list, those names are used by default.
#' @param min_scales Integer scalar. Inventories with fewer than this number of
#'   distinct scales are excluded, matching the validation logic in Wulff &
#'   Mata (2025).
#' @param self_cutoff Numeric scalar passed to `get_item_scale_cos()`. Values
#'   above this cutoff are excluded from within-scale averages, usually to
#'   remove diagonal item self-similarities.
#'
#' @return Data frame with one row per scale and model. Columns include
#'   `model`, `scale`, `inventory`, `fidelity`, `n_scales`, and logical recovery
#'   flags `recovered_full` (`fidelity > 2`) and `recovered_partial`
#'   (`fidelity > 1`).
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' set.seed(1)
#' item_cos <- diag(1, 6)
#' item_cos[item_cos == 0] <- runif(30, .1, .8)
#' item_cos <- (item_cos + t(item_cos)) / 2
#' rownames(item_cos) <- colnames(item_cos) <- paste0("item_", 1:6)
#' scale_id <- rep(paste0("inv_scale_", 1:3), each = 2)
#' names(scale_id) <- rownames(item_cos)
#' structural_fidelity(item_cos, scale_id)
#' @export
structural_fidelity <- function(
    item_cos,
    scale_id,
    inventory = NULL,
    model_names = NULL,
    min_scales = 3,
    self_cutoff = .999999) {

  item_cos_list <- embeddcv_as_named_list(item_cos, model_names, "model")
  rows <- list()
  counter <- 1L

  for (model_name in names(item_cos_list)) {
    cos <- as.matrix(item_cos_list[[model_name]])
    item_ids <- rownames(cos)
    if (is.null(item_ids) || is.null(colnames(cos))) {
      stop("Each matrix in `item_cos` must have row and column names.", call. = FALSE)
    }
    if (!identical(item_ids, colnames(cos))) {
      stop("Each matrix in `item_cos` must have aligned row and column names.", call. = FALSE)
    }

    scale_vec <- embeddcv_match_item_vector(scale_id, item_ids, "scale_id")
    inventory_vec <- if (is.null(inventory)) {
      instrument_from_scale(scale_vec)
    } else {
      embeddcv_match_item_vector(inventory, item_ids, "inventory")
    }

    scale_inventory <- stats::aggregate(
      inventory_vec,
      by = list(scale = scale_vec),
      FUN = function(x) {
        ux <- unique(stats::na.omit(as.character(x)))
        if (length(ux) == 0) NA_character_ else ux[1]
      }
    )
    names(scale_inventory)[2] <- "inventory"

    scale_cos <- get_item_scale_cos(cos, scale_vec, self_cutoff = self_cutoff)
    scale_inventory <- scale_inventory[match(rownames(scale_cos), scale_inventory$scale), ]
    inventories <- unique(scale_inventory$inventory)

    for (inventory_name in inventories) {
      sel <- scale_inventory$scale[scale_inventory$inventory == inventory_name]
      sel <- sel[sel %in% rownames(scale_cos)]
      if (length(sel) < min_scales) next

      sub <- scale_cos[sel, sel, drop = FALSE]
      for (scale_name in sel) {
        z <- embeddcv_z(sub[scale_name, ])
        rows[[counter]] <- data.frame(
          model = model_name,
          scale = scale_name,
          inventory = inventory_name,
          fidelity = unname(z[scale_name]),
          n_scales = length(sel),
          recovered_full = isTRUE(unname(z[scale_name]) > 2),
          recovered_partial = isTRUE(unname(z[scale_name]) > 1),
          stringsAsFactors = FALSE
        )
        counter <- counter + 1L
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      model = character(),
      scale = character(),
      inventory = character(),
      fidelity = numeric(),
      n_scales = integer(),
      recovered_full = logical(),
      recovered_partial = logical()
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot structural fidelity scores
#'
#' Draws a compact bar plot of structural fidelity scores. Each bar is a scale
#' under one embedding model. The dashed horizontal line marks the
#' \eqn{z = 2} full-recovery criterion, and the layout follows the
#' structural-fidelity analysis used by Wulff & Mata (2025).
#'
#' When more than one model is present, bars for the same scale are drawn in the
#' model order with decreasing width. This makes later models visible in front
#' of earlier models while preserving a shared scale ordering. The common order
#' is taken from the first model in `fidelity_tbl`.
#'
#' Scale labels are drawn only when the table contains one model. With two or
#' more models, scales share the same x positions across models; drawing one
#' label row can be misleading because the order is a common comparison order,
#' not a model-specific ranking. If `show_scale_labels = TRUE` and two or more
#' models are present, the function issues a warning and sets
#' `show_scale_labels = FALSE`.
#'
#' @param fidelity_tbl Data frame returned by `structural_fidelity()`. It must
#'   contain `model`, `scale`, `inventory`, and `fidelity`.
#' @param output_file Optional character path. If supplied, the plot is written
#'   with `ggplot2::ggsave()`.
#' @param width,height,dpi Numeric scalars passed to `ggplot2::ggsave()` when
#'   `output_file` is not `NULL`.
#' @param palette Character scalar. Color palette for model bars. Options are
#'   `"wulff_mata"`, `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`,
#'   `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#' @param show_legend Logical scalar. If `TRUE`, shows a model legend. The
#'   default is `FALSE`, which is cleaner when only one model is plotted.
#' @param show_scale_labels Logical scalar. If `TRUE`, prints scale names close
#'   to the bottom of the bars. This is supported only when `fidelity_tbl`
#'   contains one model. If two or more models are present, the function warns
#'   and changes this argument to `FALSE` to avoid implying model-specific scale
#'   ranks.
#' @param scale_label_fun Function used to convert scale ids into displayed
#'   axis labels when `show_scale_labels = TRUE`. The default keeps ids as-is.
#' @param scale_label_size Optional numeric text size for scale labels. If
#'   `NULL`, the size is chosen from `width` and the number of scales so the
#'   rotated labels are narrower than the bar spacing.
#' @param scale_label_margin Numeric scalar. Vertical gap, in data units,
#'   between the x axis and the rotated scale labels when `show_scale_labels =
#'   TRUE`. Smaller values move labels closer to the bars.
#' @param scale_tick_length Numeric scalar, in points. Length of x-axis tick
#'   marks when scale labels are shown.
#' @param recovery_line Numeric scalar. Horizontal criterion line. The default
#'   is `2`, matching the full-recovery threshold in Wulff & Mata (2025).
#' @param recovery_line_col Character scalar. Color of the recovery criterion
#'   line.
#' @param recovery_label_col Character scalar. Color of the recovery text.
#' @param recovery_label_fill Character scalar. Fill color of the recovery text
#'   box.
#' @param below_recovery_fill Optional character scalar. If `NULL` (default),
#'   bars below `recovery_line` keep the same model color as the remaining
#'   bars. Supply a color to override their fill.
#' @param show_below_recovery_stripes Logical scalar. If `TRUE`, draws
#'   horizontal stripes over bars below `recovery_line`. The default is `FALSE`,
#'   so bars below the recovery criterion keep the same visual treatment as the
#'   other bars while the dashed recovery line remains visible.
#' @param below_recovery_stripe_col Character scalar. Color of horizontal
#'   stripes drawn over bars below `recovery_line`. Set to `NA` to suppress
#'   stripes even when `show_below_recovery_stripes = TRUE`.
#' @param plot_margin Optional plot margin in points. If `NULL`, margins are
#'   chosen automatically from the label and legend settings. Supply one
#'   numeric value for all sides, a numeric vector in top-right-bottom-left
#'   order, or a `ggplot2::margin()` object.
#' @param y_limits Optional numeric vector of length 2. If `NULL`, limits are
#'   chosen from the data and `recovery_line`.
#'
#' @return A ggplot object. If `output_file` is supplied, the same plot is saved
#'   to disk.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
plot_structural_fidelity <- function(
    fidelity_tbl,
    output_file = NULL,
    width = 9,
    height = 4.8,
    dpi = 600,
    palette = "wulff_mata",
    show_legend = FALSE,
    show_scale_labels = FALSE,
    scale_label_fun = identity,
    scale_label_size = NULL,
    scale_label_margin = .015,
    scale_tick_length = .5,
    recovery_line = 2,
    recovery_line_col = "#D8B365",
    recovery_label_col = "#B2182B",
    recovery_label_fill = "white",
    below_recovery_fill = NULL,
    show_below_recovery_stripes = FALSE,
    below_recovery_stripe_col = "grey55",
    plot_margin = NULL,
    y_limits = NULL) {

  dat <- fidelity_tbl
  if (!all(c("model", "scale", "inventory", "fidelity") %in% names(dat))) {
    stop("`fidelity_tbl` must contain `model`, `scale`, `inventory`, and `fidelity`.", call. = FALSE)
  }
  dat <- dat[is.finite(dat$fidelity), , drop = FALSE]

  dat$model <- factor(dat$model, levels = unique(dat$model))
  model_levels <- levels(dat$model)

  order_source <- dat[dat$model == model_levels[1], c("scale", "fidelity"), drop = FALSE]
  order_source <- order_source[!duplicated(order_source$scale), , drop = FALSE]
  order_source <- order_source[order(-order_source$fidelity, order_source$scale), , drop = FALSE]
  missing_order_scales <- setdiff(unique(dat$scale), order_source$scale)
  scale_levels <- c(order_source$scale, sort(missing_order_scales))
  dat$scale_order <- factor(dat$scale, levels = scale_levels)
  dat$scale_x <- match(dat$scale, scale_levels)

  if (isTRUE(show_scale_labels) && length(model_levels) > 1) {
    warning(
      "`show_scale_labels = TRUE` is only supported when `fidelity_tbl` contains one model. ",
      "Two or more models share the same x positions, so one label row can imply a ",
      "model-specific ranking that the plot does not use. Setting `show_scale_labels = FALSE`.",
      call. = FALSE
    )
    show_scale_labels <- FALSE
  }

  cols <- embeddcv_relabel_palette(length(model_levels), palette = palette, begin = .1, end = .9)
  names(cols) <- model_levels
  if (is.null(scale_label_size)) {
    scale_label_size <- if (isTRUE(show_scale_labels)) {
      bar_pitch_mm <- width * 25.4 * .72 / length(scale_levels)
      max(.45, min(1.25, bar_pitch_mm * .62))
    } else {
      11
    }
  }

  if (is.null(y_limits)) {
    y_limits <- range(c(0, recovery_line, dat$fidelity), na.rm = TRUE)
    y_limits[2] <- max(3, y_limits[2] * 1.08)
  }

  scale_label_values <- as.character(scale_label_fun(scale_levels))
  axis_title <- paste0(
    "Scales ordered by structural fidelity\n",
    "(N = ", length(scale_levels), ")"
  )
  max_label_chars <- max(nchar(scale_label_values), na.rm = TRUE)
  label_depth_pt <- if (isTRUE(show_scale_labels)) {
    max(24, max_label_chars * scale_label_size * 1.25)
  } else {
    0
  }
  bottom_margin_pt <- if (isTRUE(show_legend)) 10 else 6

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$scale_x, y = .data$fidelity))
  bar_widths <- if (length(model_levels) == 1) {
    .75
  } else {
    seq(.9, .42, length.out = length(model_levels))
  }
  for (i in seq_along(model_levels)) {
    sub <- dat[dat$model == model_levels[i], , drop = FALSE]
    sub$fill_group <- as.character(sub$model)
    if (!is.null(below_recovery_fill)) {
      sub$fill_group[sub$fidelity < recovery_line] <- "Below recovery"
    }
    p <- p + ggplot2::geom_col(
      data = sub,
      ggplot2::aes(fill = .data$fill_group),
      width = bar_widths[i],
      alpha = if (length(model_levels) == 1) 1 else .72
    )

    stripe_tbl <- embeddcv_structural_fidelity_stripes(
      sub[sub$fidelity < recovery_line, , drop = FALSE],
      width = bar_widths[i],
      x_range = length(scale_levels),
      y_range = diff(y_limits),
      plot_aspect = width / height
    )
    if (isTRUE(show_below_recovery_stripes) && !is.na(below_recovery_stripe_col) && nrow(stripe_tbl) > 0) {
      p <- p + ggplot2::geom_segment(
        data = stripe_tbl,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
        inherit.aes = FALSE,
        linewidth = 1.44,
        colour = below_recovery_stripe_col,
        alpha = .95,
        lineend = "butt"
      )
    }
  }

  fill_values <- cols
  if (!is.null(below_recovery_fill)) {
    fill_values <- c(fill_values, `Below recovery` = below_recovery_fill)
  }
  p <- p +
    ggplot2::geom_hline(yintercept = recovery_line, linetype = 2, linewidth = .45, colour = recovery_line_col) +
    ggplot2::annotate(
      "label",
      x = 1,
      y = recovery_line,
      label = "Successful recovery",
      hjust = 0,
      vjust = 1.25,
      size = 3,
      colour = recovery_label_col,
      fill = recovery_label_fill,
      alpha = .85
    ) +
    ggplot2::scale_fill_manual(values = fill_values, name = NULL, breaks = model_levels) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(scale_levels),
      labels = rep("", length(scale_levels)),
      expand = ggplot2::expansion(mult = c(.001, .001))
    ) +
    ggplot2::coord_cartesian(ylim = y_limits, clip = "off") +
    ggplot2::labs(
      x = axis_title,
      y = expression("Structural fidelity (" * italic(z) * ")")
    )

  if (isTRUE(show_scale_labels)) {
    scale_label_tbl <- data.frame(
      scale_x = seq_along(scale_levels),
      label = scale_label_values,
      stringsAsFactors = FALSE
    )
    p <- p + ggplot2::geom_text(
      data = scale_label_tbl,
      ggplot2::aes(x = .data$scale_x, y = y_limits[1] - scale_label_margin, label = .data$label),
      inherit.aes = FALSE,
      angle = 90,
      hjust = 1,
      vjust = .5,
      size = scale_label_size,
      colour = "grey20"
    )
  }

  auto_plot_margin <- ggplot2::margin(5.5, 5.5, bottom_margin_pt, 5.5)
  plot_margin_value <- embeddcv_plot_margin(plot_margin, auto_plot_margin)

  p <- p +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = if (isTRUE(show_scale_labels)) {
        ggplot2::element_line(linewidth = .15, colour = "grey45")
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.length.x = grid::unit(if (isTRUE(show_scale_labels)) scale_tick_length else 0, "pt"),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = if (isTRUE(show_scale_labels)) label_depth_pt + 4 else 4)
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if (isTRUE(show_legend)) "bottom" else "none",
      plot.margin = plot_margin_value
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}

embeddcv_plot_margin <- function(plot_margin, default_margin) {
  if (is.null(plot_margin)) {
    return(default_margin)
  }
  if (inherits(plot_margin, "unit")) {
    return(plot_margin)
  }
  if (!is.numeric(plot_margin) || !length(plot_margin) %in% c(1, 4)) {
    stop("`plot_margin` must be NULL, a numeric vector of length 1 or 4, or a ggplot2::margin() object.", call. = FALSE)
  }
  if (length(plot_margin) == 1) {
    plot_margin <- rep(plot_margin, 4)
  }
  ggplot2::margin(plot_margin[1], plot_margin[2], plot_margin[3], plot_margin[4])
}

embeddcv_structural_fidelity_stripes <- function(dat, width, x_range, y_range, plot_aspect, gap = .22) {
  if (nrow(dat) == 0) {
    return(data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric()))
  }

  rows <- list()
  counter <- 1L
  half_width <- width / 2

  for (i in seq_len(nrow(dat))) {
    x_left <- dat$scale_x[i] - half_width
    x_right <- dat$scale_x[i] + half_width
    y_top <- dat$fidelity[i]
    if (!is.finite(y_top) || y_top == 0) next

    y_min <- min(0, y_top)
    y_max <- max(0, y_top)
    y_start <- y_min + gap / 2
    y_end <- y_max - gap / 2
    if (y_start <= y_end) {
      y_positions <- seq(y_start, y_end, by = gap)
    } else {
      y_positions <- mean(c(y_min, y_max))
    }

    for (y0 in y_positions) {
      if (is.finite(y0)) {
        if (y0 >= y_min && y0 <= y_max) {
          rows[[counter]] <- data.frame(x = x_left, y = y0, xend = x_right, yend = y0)
          counter <- counter + 1L
        }
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric()))
  }

  do.call(rbind, rows)
}

#' Compute scale-label alignment scores
#'
#' Computes the scale-label alignment score used by Wulff & Mata (2025). The
#' score asks whether each scale is more semantically similar to its assigned
#' label or definition than to other candidate labels or definitions. Multiple
#' definition sets can be supplied and named, making it possible to compare
#' labels, curated definitions, GPT-generated definitions, or any user-provided
#' label embeddings.
#'
#' @details
#' For scale \eqn{j}, label or definition set \eqn{m}, and candidate label
#' \eqn{\ell}, let \eqn{S_{j\ell m}} be the cosine similarity between the scale
#' embedding and candidate label embedding. If \eqn{L(j)} is the assigned
#' label or set of labels for scale \eqn{j}, the alignment score is
#'
#' \deqn{
#' \operatorname{alignment}_{jm} =
#' \operatorname{mean}_{\ell \in L(j)}
#' \left[
#' \frac{S_{j\ell m} - N_m^{-1}\sum_{q=1}^{N_m} S_{jqm}}
#' {\sqrt{\frac{1}{N_m - 1}\sum_{r=1}^{N_m}
#' \left(S_{jrm} - N_m^{-1}\sum_{q=1}^{N_m} S_{jqm}\right)^2}}
#' \right],
#' }
#'
#' where \eqn{N_m} is the number of candidate labels in set \eqn{m}. This is the
#' same row-wise z-score used by Wulff & Mata (2025), written with \eqn{m} to
#' allow several user-supplied label or definition sets. The plotted value is
#' usually the mean of `alignment` across scales for each definition set.
#'
#' @param scale_emb Numeric matrix of scale embeddings. Rows are scales and
#'   columns are embedding dimensions. Required when `scale_label_cross = NULL`.
#' @param label_embeddings Numeric matrix of label/definition embeddings, or a
#'   list of such matrices. Rows are candidate labels/definitions and columns
#'   are embedding dimensions. Required when `scale_label_cross = NULL`.
#' @param scale_label_cross Optional numeric scale-by-label similarity matrix,
#'   or a list of such matrices. Rows are scales and columns are candidate
#'   labels. If supplied, `scale_emb` and `label_embeddings` are not used.
#' @param label_by_scale Named character vector or named list mapping scale ids
#'   to their assigned label ids. Names must be scale ids; values must be
#'   candidate labels in the columns of each cross-similarity matrix. Character
#'   values containing `label_sep` are split into multiple assigned labels.
#' @param definition_names Optional character vector used to label the
#'   definition/label sets in the returned table and plot. For example, use
#'   `"B5"` when the only supplied definition set is the B5/PD definitions.
#' @param definition_groups Optional character vector with one group label per
#'   definition/label set. This is useful when several sets belong to the same
#'   source family, such as `"Labels"`, `"APA definitions"`, and
#'   `"GPT-4 definitions"` in Wulff & Mata (2025). If `NULL`, each definition
#'   set is treated as its own group.
#' @param label_sep Character scalar used to split multiple assigned labels in
#'   `label_by_scale` values.
#'
#' @return Data frame with one row per scale and definition set. Columns include
#'   `definition_set`, `scale`, `assigned_label`, and `alignment`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
scale_label_alignment <- function(
    scale_emb = NULL,
    label_embeddings = NULL,
    scale_label_cross = NULL,
    label_by_scale,
    definition_names = NULL,
    definition_groups = NULL,
    label_sep = "/") {

  if (is.null(scale_label_cross)) {
    if (is.null(scale_emb) || is.null(label_embeddings)) {
      stop("Supply either `scale_label_cross` or both `scale_emb` and `label_embeddings`.", call. = FALSE)
    }
    label_list <- embeddcv_as_named_list(label_embeddings, definition_names, "definition_set")
    cross_list <- lapply(label_list, function(label_emb) {
      embeddcv_cross_cosine(scale_emb, label_emb)
    })
  } else {
    cross_list <- embeddcv_as_named_list(scale_label_cross, definition_names, "definition_set")
  }

  if (!is.null(definition_names)) {
    if (length(definition_names) != length(cross_list)) {
      stop("`definition_names` must have one value per label/definition set.", call. = FALSE)
    }
    names(cross_list) <- definition_names
  }

  if (is.null(definition_groups)) {
    definition_groups <- names(cross_list)
  } else if (length(definition_groups) != length(cross_list)) {
    stop("`definition_groups` must have one value per label/definition set.", call. = FALSE)
  }
  names(definition_groups) <- names(cross_list)

  if (is.null(names(label_by_scale))) {
    stop("`label_by_scale` must be named by scale id.", call. = FALSE)
  }

  rows <- list()
  counter <- 1L

  for (set_name in names(cross_list)) {
    cross <- as.matrix(cross_list[[set_name]])
    if (is.null(rownames(cross)) || is.null(colnames(cross))) {
      stop("Each cross-similarity matrix must have row and column names.", call. = FALSE)
    }

    scales <- intersect(names(label_by_scale), rownames(cross))
    for (scale_name in scales) {
      assigned <- embeddcv_assigned_labels(label_by_scale[[scale_name]], label_sep)
      assigned <- assigned[assigned %in% colnames(cross)]
      if (length(assigned) == 0) next

      z <- embeddcv_z(cross[scale_name, ])
      rows[[counter]] <- data.frame(
        definition_set = set_name,
        definition_group = unname(definition_groups[[set_name]]),
        scale = scale_name,
        assigned_label = paste(assigned, collapse = label_sep),
        alignment = mean(z[assigned], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      counter <- counter + 1L
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      definition_set = character(),
      definition_group = character(),
      scale = character(),
      assigned_label = character(),
      alignment = numeric()
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot scale-label alignment scores
#'
#' Plots the mean scale-label alignment score for each supplied label or
#' definition set. Each point is the average z-score alignment across scales.
#' The layout follows the scale-label alignment analysis shown by Wulff & Mata
#' (2025). Guide-line colors are lightened with `mix_col()`, a local color
#' mixer adapted from `memnet::cmix()` (Wulff, 2019).
#'
#' @param alignment_tbl Data frame returned by `scale_label_alignment()`. It
#'   must contain `definition_set` and `alignment`; if it also contains
#'   `definition_group`, points are arranged within those x-axis groups.
#' @param output_file Optional character path. If supplied, the plot is written
#'   with `ggplot2::ggsave()`.
#' @param width,height,dpi Numeric scalars passed to `ggplot2::ggsave()` when
#'   `output_file` is not `NULL`.
#' @param palette Character scalar. Color palette for points. Options are
#'   `"wulff_mata"`, `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`,
#'   `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#' @param y_limits Optional numeric vector of length 2. The default covers 0 to
#'   at least 3.5, matching the scale used by Wulff & Mata (2025) when values
#'   are in that range.
#' @param point_size Numeric scalar. Size of the square points.
#'
#' @return A ggplot object. If `output_file` is supplied, the same plot is saved
#'   to disk.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' Wulff, D. U. (2019). memnet: Network Tools for Memory Research. R package.
#' https://www.rdocumentation.org/packages/memnet/versions/0.1.0
#'
#' @export
plot_scale_label_alignment <- function(
    alignment_tbl,
    output_file = NULL,
    width = 6.5,
    height = 4.2,
    dpi = 300,
    palette = "wulff_mata",
    y_limits = NULL,
    point_size = 10.5) {

  if (!all(c("definition_set", "alignment") %in% names(alignment_tbl))) {
    stop("`alignment_tbl` must contain `definition_set` and `alignment`.", call. = FALSE)
  }

  if (!"definition_group" %in% names(alignment_tbl)) {
    alignment_tbl$definition_group <- alignment_tbl$definition_set
  }

  dat <- stats::aggregate(
    alignment ~ definition_group + definition_set,
    alignment_tbl,
    function(x) mean(x, na.rm = TRUE)
  )
  names(dat)[3] <- "mean_alignment"
  dat <- dat[order(match(dat$definition_group, unique(alignment_tbl$definition_group)),
                   match(dat$definition_set, unique(alignment_tbl$definition_set))), , drop = FALSE]
  dat$definition_set <- factor(dat$definition_set, levels = unique(alignment_tbl$definition_set))
  dat$definition_group <- factor(dat$definition_group, levels = unique(dat$definition_group))
  cols <- embeddcv_relabel_palette(nrow(dat), palette = palette, begin = .1, end = .9)
  names(cols) <- as.character(dat$definition_set)

  group_levels <- levels(dat$definition_group)
  dat$x <- NA_real_
  for (i in seq_along(group_levels)) {
    idx <- which(dat$definition_group == group_levels[i])
    offsets <- if (length(idx) == 1) 0 else seq(-.25, .25, length.out = length(idx))
    dat$x[idx] <- i + offsets
  }
  group_breaks <- stats::aggregate(x ~ definition_group, dat, mean)

  if (is.null(y_limits)) {
    y_limits <- c(0, max(3.5, dat$mean_alignment, na.rm = TRUE) * 1.04)
  }

  guide_lines <- data.frame(y = seq(0, floor(y_limits[2]), by = 1))
  guide_lines <- guide_lines[guide_lines$y > y_limits[1] & guide_lines$y < y_limits[2], , drop = FALSE]

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$x, y = .data$mean_alignment)) +
    ggplot2::geom_hline(
      data = guide_lines,
      ggplot2::aes(yintercept = .data$y),
      inherit.aes = FALSE,
      linetype = 2,
      linewidth = .25,
      colour = mix_col(cols[1], "white", .65)
    ) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$definition_set), shape = 22, size = point_size, colour = "white", stroke = .7) +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = group_breaks$x,
      labels = as.character(group_breaks$definition_group),
      limits = c(.5, length(group_levels) + .5)
    ) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(x = NULL, y = "Scale-label alignment") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = .5, vjust = .5),
      legend.position = "none"
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}

#' Compute semantic internal consistency
#'
#' Computes the semantic analogue of internal consistency used by Wulff & Mata
#' (2025). The function estimates a predicted Cronbach's alpha from item-item
#' semantic similarities. It does not require participant responses; therefore,
#' the returned value should be interpreted as semantic internal consistency, or
#' predicted alpha, rather than as an observed reliability coefficient.
#'
#' @details
#' For scale \eqn{j} with item set \eqn{I_j}, let \eqn{C_{ab}} be the cosine
#' similarity between item embeddings \eqn{a} and \eqn{b}. The within-scale
#' semantic similarity is the mean over distinct item pairs:
#'
#' \deqn{
#' \bar{S}_j =
#' \operatorname{mean}\{C_{ab}: a,b \in I_j,\ a \ne b\}.
#' }
#'
#' The predicted internal consistency is then computed with the Spearman-Brown
#' prediction formula:
#'
#' \deqn{
#' \hat{\alpha}_j =
#' \frac{n_j\bar{S}_j}{1 + (n_j - 1)\bar{S}_j},
#' }
#'
#' where \eqn{n_j} is the number of items in scale \eqn{j}. Wulff & Mata (2025)
#' compared \eqn{\hat{\alpha}_j} against empirical Cronbach's alpha values from
#' IPIP. When no empirical alpha is supplied, this function still provides the
#' semantic prediction but cannot evaluate prediction accuracy.
#'
#' @param item_cos Numeric square item-item similarity matrix, or a named list
#'   of such matrices. Rows and columns must represent the same items. These
#'   matrices are typically produced by `cosine_matrix(item_embeddings)`.
#' @param scale_id Character vector assigning each item to a scale. If the vector
#'   has names, they are matched to the row names of each matrix in `item_cos`;
#'   otherwise the vector must be in the same order as the matrix rows.
#' @param model_names Optional character vector used to name the similarity
#'   matrices when `item_cos` is not already a named list.
#' @param self_cutoff Numeric scalar. Similarities greater than this value are
#'   removed before computing \eqn{\bar{S}_j}; this removes diagonal
#'   self-similarities that are equal or extremely close to 1.
#'
#' @return Data frame with one row per scale and model. Columns include `model`,
#'   `scale`, `n_items`, `mean_item_similarity`, and `alpha_semantic`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' item_cos <- diag(1, 4)
#' item_cos[item_cos == 0] <- .35
#' rownames(item_cos) <- colnames(item_cos) <- paste0("item_", 1:4)
#' semantic_alpha(item_cos, scale_id = c("scale_a", "scale_a", "scale_b", "scale_b"))
#' @export
semantic_alpha <- function(
    item_cos,
    scale_id,
    model_names = NULL,
    self_cutoff = .999999) {

  item_cos_list <- embeddcv_as_named_list(item_cos, model_names, "model")
  rows <- list()
  counter <- 1L

  for (model_name in names(item_cos_list)) {
    cos <- as.matrix(item_cos_list[[model_name]])
    item_ids <- rownames(cos)
    if (is.null(item_ids) || is.null(colnames(cos))) {
      stop("Each matrix in `item_cos` must have row and column names.", call. = FALSE)
    }
    if (!identical(item_ids, colnames(cos))) {
      stop("Each matrix in `item_cos` must have aligned row and column names.", call. = FALSE)
    }

    scale_vec <- embeddcv_match_item_vector(scale_id, item_ids, "scale_id")
    scale_cos <- get_item_scale_cos(cos, scale_vec, self_cutoff = self_cutoff)
    n_items <- table(scale_vec)
    common_scales <- intersect(rownames(scale_cos), names(n_items))

    for (scale_name in common_scales) {
      n <- as.integer(n_items[[scale_name]])
      mean_similarity <- unname(scale_cos[scale_name, scale_name])
      alpha_semantic <- if (is.finite(mean_similarity) && n > 1) {
        n * mean_similarity / (1 + (n - 1) * mean_similarity)
      } else {
        NA_real_
      }

      rows[[counter]] <- data.frame(
        model = model_name,
        scale = scale_name,
        n_items = n,
        mean_item_similarity = mean_similarity,
        alpha_semantic = alpha_semantic,
        stringsAsFactors = FALSE
      )
      counter <- counter + 1L
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      model = character(),
      scale = character(),
      n_items = integer(),
      mean_item_similarity = numeric(),
      alpha_semantic = numeric()
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Compute convergent-validity recovery from scale similarities
#'
#' Computes the standardized semantic similarity among scales assigned to the
#' same construct label. The calculation follows the convergent-validity
#' analysis described by Wulff & Mata (2025): scales that share a construct label
#' should be closer to one another than to scales with different labels.
#'
#' @details
#' For construct \eqn{c}, let \eqn{G_c} be the set of scales assigned to that
#' construct. For a focal scale \eqn{j \in G_c}, the row of similarities
#' \eqn{S_{jr}} is standardized across all candidate scales:
#'
#' \deqn{
#' z_{jr} = \frac{S_{jr} - \bar{S}_{j\cdot}}{sd(S_{j\cdot})}.
#' }
#'
#' The focal-scale convergent score is the mean standardized similarity to other
#' scales with the same construct label:
#'
#' \deqn{
#' v_{jc} = \frac{1}{|G_c|-1}\sum_{r \in G_c, r \ne j} z_{jr}.
#' }
#'
#' The construct-level score returned by this function is
#' \eqn{V_c = |G_c|^{-1}\sum_{j \in G_c} v_{jc}}. Wulff & Mata (2025)
#' interpreted values above 2 as good recovery of convergent validity.
#'
#' @param scale_cos Numeric square scale-scale similarity matrix, or a named
#'   list of such matrices. Rows and columns must represent the same scales.
#' @param construct_by_scale Named character vector mapping scale ids to
#'   construct labels. If the vector is unnamed, it must be in the same order as
#'   the rows of each matrix in `scale_cos`.
#' @param model_names Optional character vector used to name the similarity
#'   matrices when `scale_cos` is not already a named list.
#' @param min_scales_per_construct Integer scalar. Constructs represented by
#'   fewer than this number of scales are excluded. Wulff & Mata (2025) used
#'   constructs present at least three times.
#'
#' @return Data frame with one row per construct and model. Columns include
#'   `model`, `construct`, `n_scales`, and `convergent_similarity`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
convergent_validity <- function(
    scale_cos,
    construct_by_scale,
    model_names = NULL,
    min_scales_per_construct = 3) {

  scale_cos_list <- embeddcv_as_named_list(scale_cos, model_names, "model")
  rows <- list()
  counter <- 1L

  for (model_name in names(scale_cos_list)) {
    cos <- as.matrix(scale_cos_list[[model_name]])
    scales <- rownames(cos)
    if (is.null(scales) || is.null(colnames(cos))) {
      stop("Each matrix in `scale_cos` must have row and column names.", call. = FALSE)
    }
    if (!identical(scales, colnames(cos))) {
      stop("Each matrix in `scale_cos` must have aligned row and column names.", call. = FALSE)
    }

    construct_vec <- embeddcv_match_scale_vector(construct_by_scale, scales, "construct_by_scale")
    names(construct_vec) <- scales
    construct_sizes <- table(construct_vec)
    constructs <- names(construct_sizes)[construct_sizes >= min_scales_per_construct]

    for (construct_name in constructs) {
      construct_scales <- names(construct_vec)[construct_vec == construct_name]
      focal_scores <- numeric(length(construct_scales))

      for (i in seq_along(construct_scales)) {
        focal <- construct_scales[i]
        other <- setdiff(construct_scales, focal)
        z <- embeddcv_z(cos[focal, ])
        focal_scores[i] <- mean(z[other], na.rm = TRUE)
      }

      rows[[counter]] <- data.frame(
        model = model_name,
        construct = construct_name,
        n_scales = length(construct_scales),
        convergent_similarity = mean(focal_scores, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      counter <- counter + 1L
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      model = character(),
      construct = character(),
      n_scales = integer(),
      convergent_similarity = numeric()
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot convergent-validity recovery
#'
#' Plots construct-level convergent-validity scores returned by
#' `convergent_validity()`. Constructs are ordered by their mean score across
#' models, matching the visual logic of the Wulff & Mata (2025) supplementary
#' convergent-validity display.
#'
#' @param convergent_tbl Data frame returned by `convergent_validity()`. It must
#'   contain `model`, `construct`, and `convergent_similarity`.
#' @param output_file Optional character path. If supplied, the plot is written
#'   with `ggplot2::ggsave()`.
#' @param width,height,dpi Numeric scalars passed to `ggplot2::ggsave()` when
#'   `output_file` is not `NULL`.
#' @param palette Character scalar. Color palette for models. Options are
#'   `"wulff_mata"`, `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`,
#'   `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#' @param show_legend Logical scalar. If `TRUE`, draws the model
#'   legend. This is useful even for one-model displays when the plotted model
#'   name should be explicit. The default is `FALSE`, matching compact
#'   single-model panels without a legend.
#' @param show_construct_labels Logical scalar. If `TRUE`, prints construct
#'   names below the x axis. The default is `FALSE`, because many constructs can
#'   make the display crowded.
#' @param construct_label_fun Function used to convert construct ids into
#'   displayed labels when `show_construct_labels = TRUE`.
#' @param construct_labels Optional character vector used to override construct
#'   labels after `construct_label_fun()` is applied. If named, names must match
#'   `convergent_tbl$construct`; if unnamed, length must equal the number of
#'   plotted constructs in their display order.
#' @param construct_label_size Optional numeric text size for construct labels.
#'   If `NULL`, the size is chosen from `width` and the number of constructs.
#' @param show_construct_n Logical scalar. If `TRUE` and `convergent_tbl`
#'   contains `n_scales`, appends the number of scales to each construct label,
#'   for example `"Antagonism (7)"`.
#' @param construct_label_margin Numeric scalar. Vertical distance between the
#'   x-axis baseline and the rotated construct labels.
#' @param construct_tick_length Numeric scalar. Length, in points, of the small
#'   x-axis ticks shown when construct labels are drawn.
#' @param point_size Numeric scalar. Size of the square points. The default is
#'   intentionally large so this display has visual weight comparable to the
#'   package's other validation panels.
#' @param point_stroke Numeric scalar. White outline width around square points.
#' @param recovery_line Numeric scalar. Horizontal reference line. The default
#'   is 2, the recovery criterion used by Wulff & Mata (2025).
#' @param y_limits Optional numeric vector of length 2. If `NULL`, limits are
#'   chosen from the data and `recovery_line`.
#'
#' @return A ggplot object. If `output_file` is supplied, the same plot is saved
#'   to disk.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
plot_convergent_validity <- function(
    convergent_tbl,
    output_file = NULL,
    width = 9,
    height = 4.8,
    dpi = 600,
    palette = "wulff_mata",
    show_legend = FALSE,
    show_construct_labels = FALSE,
    construct_label_fun = identity,
    construct_labels = NULL,
    construct_label_size = NULL,
    show_construct_n = TRUE,
    construct_label_margin = .015,
    construct_tick_length = .5,
    point_size = 10.5,
    point_stroke = .7,
    recovery_line = 2,
    y_limits = NULL) {

  if (!all(c("model", "construct", "convergent_similarity") %in% names(convergent_tbl))) {
    stop("`convergent_tbl` must contain `model`, `construct`, and `convergent_similarity`.", call. = FALSE)
  }

  dat <- convergent_tbl[is.finite(convergent_tbl$convergent_similarity), , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("`convergent_tbl` has no finite convergent-validity scores.", call. = FALSE)
  }

  construct_order <- stats::aggregate(
    convergent_similarity ~ construct,
    dat,
    function(x) mean(x, na.rm = TRUE)
  )
  construct_order <- construct_order[order(-construct_order$convergent_similarity, construct_order$construct), , drop = FALSE]
  construct_levels <- construct_order$construct
  model_levels <- unique(dat$model)
  dat$model <- factor(dat$model, levels = model_levels)
  dat$construct_x <- match(dat$construct, construct_levels)
  construct_n <- if ("n_scales" %in% names(dat)) {
    stats::aggregate(n_scales ~ construct, dat, function(x) x[which.max(!is.na(x))])
  } else {
    data.frame(construct = construct_levels, n_scales = NA_integer_)
  }
  construct_n <- construct_n[match(construct_levels, construct_n$construct), , drop = FALSE]

  if (is.null(construct_label_size)) {
    construct_label_size <- if (isTRUE(show_construct_labels)) {
      pitch_mm <- width * 25.4 * .74 / length(construct_levels)
      max(1.1, min(3.2, pitch_mm * 1.40))
    } else {
      11
    }
  }

  if (is.null(y_limits)) {
    y_limits <- range(c(0, recovery_line, dat$convergent_similarity), na.rm = TRUE)
    y_limits[2] <- max(recovery_line + .5, y_limits[2] * 1.08)
  }

  cols <- embeddcv_relabel_palette(length(model_levels), palette = palette, begin = .1, end = .9)
  names(cols) <- model_levels

  p <- ggplot2::ggplot(dat, ggplot2::aes(
    x = .data$construct_x,
    y = .data$convergent_similarity,
    fill = .data$model
  )) +
    ggplot2::geom_hline(yintercept = recovery_line, linetype = 2, linewidth = .4, colour = "#D8B365") +
    ggplot2::geom_point(shape = 22, size = point_size, colour = "white", stroke = point_stroke, alpha = .85) +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(construct_levels),
      labels = rep("", length(construct_levels)),
      expand = ggplot2::expansion(mult = c(.01, .01))
    ) +
    ggplot2::coord_cartesian(ylim = y_limits, clip = "off") +
    ggplot2::labs(
      x = paste0("Constructs ordered by convergent-validity recovery\n(N = ", length(construct_levels), ")"),
      y = expression(italic(z) * " (similarity)")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if (isTRUE(show_legend)) "bottom" else "none",
      axis.text.x = ggplot2::element_blank()
    )

  if (isTRUE(show_construct_labels)) {
    labels <- as.character(construct_label_fun(construct_levels))
    if (!is.null(construct_labels)) {
      if (!is.null(names(construct_labels))) {
        replacement <- as.character(construct_labels[construct_levels])
        labels[!is.na(replacement)] <- replacement[!is.na(replacement)]
      } else {
        if (length(construct_labels) != length(construct_levels)) {
          stop("Unnamed `construct_labels` must have one value per plotted construct.", call. = FALSE)
        }
        labels <- as.character(construct_labels)
      }
    }
    if (isTRUE(show_construct_n) && "n_scales" %in% names(construct_n)) {
      labels <- ifelse(
        is.na(construct_n$n_scales),
        labels,
        paste0(labels, " (", construct_n$n_scales, ")")
      )
    }
    label_depth_pt <- max(24, max(nchar(labels), na.rm = TRUE) * construct_label_size * 1.25)
    label_tbl <- data.frame(construct_x = seq_along(construct_levels), label = labels)
    p <- p +
      ggplot2::geom_text(
        data = label_tbl,
        ggplot2::aes(x = .data$construct_x, y = y_limits[1] - construct_label_margin, label = .data$label),
        inherit.aes = FALSE,
        angle = 90,
        hjust = 1,
        vjust = .5,
        size = construct_label_size,
        colour = "grey20"
      ) +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_line(linewidth = .15, colour = "grey45"),
        axis.ticks.length.x = grid::unit(construct_tick_length, "pt"),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = label_depth_pt + 4)),
        plot.margin = ggplot2::margin(5.5, 5.5, 8, 5.5)
      )
  }

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}

#' Compute divergent-validity recovery from empirical correlations
#'
#' Compares semantic similarities with empirical item or scale correlations. This
#' is the calculation used by Wulff & Mata (2025) for divergent validity: if a
#' semantic model recovers divergent validity, pairs of empirically unrelated
#' measures should also have low semantic similarity.
#'
#' @details
#' For each empirical pair \eqn{(a,b)}, the function extracts the predicted
#' semantic similarity \eqn{S_{ab}} from `similarity` and compares it with the
#' observed empirical criterion \eqn{|r_{ab}|}. For each model and optional
#' group, it returns:
#'
#' \deqn{\rho = \operatorname{cor}(S_{ab}, |r_{ab}|)}
#'
#' and
#'
#' \deqn{\operatorname{MAE} =
#' \frac{1}{P}\sum_{(a,b)} |S_{ab} - |r_{ab}||.}
#'
#' The observed column should therefore contain absolute empirical correlations
#' if the goal is to match Wulff & Mata (2025). If it contains signed
#' correlations, the function uses the values exactly as supplied.
#'
#' @param similarity Numeric square similarity matrix, or a named list of such
#'   matrices. Rows and columns must contain the ids referenced in
#'   `empirical_pairs`.
#' @param empirical_pairs Data frame with one row per observed item or scale
#'   pair.
#' @param id_1_col,id_2_col Character scalars naming the columns in
#'   `empirical_pairs` that contain the first and second ids.
#' @param observed_col Character scalar naming the empirical criterion column,
#'   usually an absolute correlation such as `abs_cor`.
#' @param group_col Optional character scalar naming a grouping column, such as
#'   inventory. If `NULL`, all pairs are summarized together.
#' @param model_names Optional character vector used to name the similarity
#'   matrices when `similarity` is not already a named list.
#' @param similarity_type Character scalar written to the returned `type` column,
#'   for example `"Item"` or `"Scale (avg.)"`.
#'
#' @return Data frame with one row per model and group. Columns include `model`,
#'   `group`, `type`, `n_pairs`, `correlation`, and `mae`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
divergent_validity <- function(
    similarity,
    empirical_pairs,
    id_1_col,
    id_2_col,
    observed_col = "abs_cor",
    group_col = NULL,
    model_names = NULL,
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

  similarity_list <- embeddcv_as_named_list(similarity, model_names, "model")
  groups <- if (is.null(group_col)) {
    rep("All pairs", nrow(empirical_pairs))
  } else {
    as.character(empirical_pairs[[group_col]])
  }
  group_levels <- unique(groups)
  rows <- list()
  counter <- 1L

  for (model_name in names(similarity_list)) {
    mat <- as.matrix(similarity_list[[model_name]])
    if (is.null(rownames(mat)) || is.null(colnames(mat))) {
      stop("Each matrix in `similarity` must have row and column names.", call. = FALSE)
    }

    ids_1 <- as.character(empirical_pairs[[id_1_col]])
    ids_2 <- as.character(empirical_pairs[[id_2_col]])
    available <- ids_1 %in% rownames(mat) & ids_2 %in% colnames(mat)
    pred <- rep(NA_real_, nrow(empirical_pairs))
    pred[available] <- mat[cbind(ids_1[available], ids_2[available])]
    obs <- as.numeric(empirical_pairs[[observed_col]])

    for (group_name in group_levels) {
      idx <- groups == group_name & is.finite(pred) & is.finite(obs)
      rows[[counter]] <- data.frame(
        model = model_name,
        group = group_name,
        type = similarity_type,
        n_pairs = sum(idx),
        correlation = if (sum(idx) >= 3) stats::cor(pred[idx], obs[idx]) else NA_real_,
        mae = if (sum(idx) > 0) mean(abs(pred[idx] - obs[idx])) else NA_real_,
        stringsAsFactors = FALSE
      )
      counter <- counter + 1L
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot divergent-validity recovery
#'
#' Plots Pearson correlations between semantic similarities and empirical
#' correlations from `divergent_validity()`. The display follows the
#' divergent-validity summary used by Wulff & Mata (2025), where models are
#' compared across inventories and input levels such as item and scale.
#'
#' @param divergent_tbl Data frame returned by `divergent_validity()`, or several
#'   such tables combined with `rbind()`. It must contain `model`, `group`,
#'   `type`, and `correlation`.
#' @param output_file Optional character path. If supplied, the plot is written
#'   with `ggplot2::ggsave()`.
#' @param width,height,dpi Numeric scalars passed to `ggplot2::ggsave()` when
#'   `output_file` is not `NULL`.
#' @param palette Character scalar. Color palette for `type`. Options are
#'   `"wulff_mata"`, `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`,
#'   `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#' @param show_model_labels Logical scalar. If `TRUE` (default), prints model
#'   names on the x axis.
#' @param model_label_fun Function used to convert model ids into displayed
#'   labels.
#' @param y_limits Optional numeric vector of length 2.
#'
#' @return A ggplot object. If `output_file` is supplied, the same plot is saved
#'   to disk.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
plot_divergent_validity <- function(
    divergent_tbl,
    output_file = NULL,
    width = 8,
    height = 4,
    dpi = 600,
    palette = "wulff_mata",
    show_model_labels = TRUE,
    model_label_fun = identity,
    y_limits = c(-.3, 1)) {

  if (!all(c("model", "group", "type", "correlation") %in% names(divergent_tbl))) {
    stop("`divergent_tbl` must contain `model`, `group`, `type`, and `correlation`.", call. = FALSE)
  }

  dat <- divergent_tbl[is.finite(divergent_tbl$correlation), , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("`divergent_tbl` has no finite correlations to plot.", call. = FALSE)
  }

  dat$model <- factor(dat$model, levels = unique(dat$model))
  dat$group <- factor(dat$group, levels = unique(dat$group))
  dat$type <- factor(dat$type, levels = unique(dat$type))
  type_levels <- levels(dat$type)
  cols <- embeddcv_relabel_palette(length(type_levels), palette = palette, begin = .1, end = .9)
  names(cols) <- type_levels
  x_labels <- if (isTRUE(show_model_labels)) {
    as.character(model_label_fun(levels(dat$model)))
  } else {
    rep("", length(levels(dat$model)))
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(
    x = .data$model,
    y = .data$correlation,
    colour = .data$type,
    group = .data$type
  )) +
    ggplot2::geom_line(linewidth = .45, alpha = .75) +
    ggplot2::geom_point(shape = 15, size = 3.6, alpha = .75) +
    ggplot2::facet_wrap(~group, ncol = length(levels(dat$group))) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::scale_x_discrete(labels = x_labels) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(x = NULL, y = "Correlation") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5),
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
#' Draws a polished histogram for a vector, matrix, or data frame of cosine
#' similarities. The function is useful for inspecting item-item, scale-scale,
#' definition-definition, or cross-set similarities before choosing thresholds
#' or running downstream analyses.
#'
#' @details
#' Numeric vectors are plotted directly. For square matrices, the default
#' `matrix_triangle = "upper"` uses only the upper triangle, which removes the
#' diagonal self-similarities and avoids counting symmetric pairs twice. For
#' rectangular matrices, all finite values are used. For data frames, supply
#' `value_col`; if it is omitted, the function looks for a column named
#' `cosim`, `similarity`, `value`, `cosine`, or `cos`.
#'
#' When `show_quartiles = TRUE`, dashed vertical lines mark Q25, Q50, and Q75.
#' Labels are printed below the x axis with the quantile name and the
#' corresponding value. When `show_percentiles = TRUE`, percentile markers are
#' also drawn. By default these are P10, P30, P50, P70, and P90; set
#' `all_deciles = TRUE` to show P10 through P90.
#'
#' @param similarities Numeric vector, matrix, array, or data frame containing
#'   cosine similarities. Matrix row and column names are optional.
#' @param value_col Optional character scalar naming the similarity column when
#'   `similarities` is a data frame.
#' @param output_file Optional character path. If supplied, the plot is written
#'   with `ggplot2::ggsave()`.
#' @param width,height,dpi Numeric scalars passed to `ggplot2::ggsave()` when
#'   `output_file` is not `NULL`.
#' @param bins Integer scalar. Number of histogram bins.
#' @param palette Character scalar. Color palette. Options are `"wulff_mata"`,
#'   `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`,
#'   `"okabe_ito"`, and `"blue_gold"`. This is kept for backward
#'   compatibility; the default histogram, density, and marker colors are
#'   controlled by `hist_fill`, `density_col`, and `marker_col`.
#' @param hist_fill Character scalar. Fill color for histogram bars.
#' @param density_col Character scalar. Color for the density curve.
#' @param marker_col Character scalar. Color for quartile/percentile dashed
#'   lines and their bold labels.
#' @param title,subtitle Optional character scalars for the plot title and
#'   subtitle. If `subtitle = NULL`, a compact summary with N, mean, and SD is
#'   generated automatically.
#' @param x_label,y_label Axis labels.
#' @param show_quartiles Logical scalar. If `TRUE`, draws Q1, Q2, and Q3.
#' @param show_percentiles Logical scalar. If `TRUE`, draws percentile markers.
#' @param percentiles Numeric vector of probabilities in 0-1 scale, or
#'   percentages in 0-100 scale. Used when `show_percentiles = TRUE` and
#'   `all_deciles = FALSE`.
#' @param all_deciles Logical scalar. If `TRUE`, ignores `percentiles` and uses
#'   P10, P20, ..., P90.
#' @param matrix_triangle Character scalar, one of `"upper"`, `"lower"`, or
#'   `"all"`. Used only for square matrices.
#' @param line_label_digits Integer scalar. Number of decimal places shown under
#'   quartile and percentile labels.
#'
#' @return A ggplot object. If `output_file` is supplied, the same plot is saved
#'   to disk.
#'
#' @examples
#' x <- rbeta(400, 4, 6)
#' plot_similarity_distribution(x)
#' plot_similarity_distribution(x, show_percentiles = TRUE, show_quartiles = FALSE)
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

embeddcv_as_named_list <- function(x, names_override = NULL, default_prefix = "set") {
  if (is.list(x) && !is.data.frame(x) && !is.matrix(x)) {
    out <- x
  } else {
    out <- list(x)
  }

  if (!is.null(names_override)) {
    if (length(names_override) != length(out)) {
      stop("Name vector length must match the number of supplied objects.", call. = FALSE)
    }
    names(out) <- names_override
  }

  if (is.null(names(out)) || any(names(out) == "")) {
    names(out) <- paste0(default_prefix, "_", seq_along(out))
  }

  out
}

embeddcv_match_item_vector <- function(x, item_ids, arg_name) {
  if (is.null(names(x))) {
    if (length(x) != length(item_ids)) {
      stop("`", arg_name, "` must have length `nrow(item_cos)` or names matching row names.", call. = FALSE)
    }
    return(as.character(x))
  }

  out <- as.character(x[item_ids])
  if (anyNA(out)) {
    stop("`", arg_name, "` is missing values for some item ids.", call. = FALSE)
  }
  out
}

embeddcv_match_scale_vector <- function(x, scale_ids, arg_name) {
  if (is.null(names(x))) {
    if (length(x) != length(scale_ids)) {
      stop("`", arg_name, "` must have length `nrow(scale_cos)` or names matching row names.", call. = FALSE)
    }
    return(as.character(x))
  }

  out <- as.character(x[scale_ids])
  if (anyNA(out)) {
    stop("`", arg_name, "` is missing values for some scale ids.", call. = FALSE)
  }
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

embeddcv_cross_cosine <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  storage.mode(x) <- "double"
  storage.mode(y) <- "double"
  if (is.null(rownames(x)) || is.null(rownames(y))) {
    stop("`scale_emb` and `label_embeddings` must have row names.", call. = FALSE)
  }
  if (ncol(x) != ncol(y)) {
    stop("Scale and label embeddings must have the same number of columns.", call. = FALSE)
  }

  x_norm <- sqrt(rowSums(x * x))
  y_norm <- sqrt(rowSums(y * y))
  x_norm[x_norm == 0] <- NA_real_
  y_norm[y_norm == 0] <- NA_real_
  out <- tcrossprod(x / x_norm, y / y_norm)
  rownames(out) <- rownames(x)
  colnames(out) <- rownames(y)
  out
}

embeddcv_assigned_labels <- function(x, label_sep = "/") {
  if (is.list(x)) {
    return(as.character(unlist(x, use.names = FALSE)))
  }
  x <- as.character(x)
  x <- unlist(strsplit(x, label_sep, fixed = TRUE), use.names = FALSE)
  x[nzchar(x)]
}
