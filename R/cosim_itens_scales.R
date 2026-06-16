#' Cosine Similarity and Complexity Measures between Items and Scales
#'
#' Computes the cosine similarity between item embeddings and scale embeddings,
#' then summarises each item's similarity profile (best/second-best scale,
#' Hoffman's complexity, Hoyer's sparsity, within-row SD). This is the original
#' item-by-scale workflow. For the general case of comparing any two embedding
#' sets (scale-scale, scale-label, definition-definition, ...) use
#' \code{\link{cosim_xy}}.
#'
#' @param item_emb Numeric matrix or data frame. Item embeddings (rows = items,
#'   cols = embedding dimensions).
#' @param scale_emb Numeric matrix or data frame. Scale embeddings (rows =
#'   scales, cols = embedding dimensions).
#' @param item_text Character vector of length \code{nrow(item_emb)}. Item text
#'   or labels.
#' @param factor_itens Character/factor vector of length \code{nrow(item_emb)}.
#'   Group (scale/factor) each item belongs to.
#' @param factor_scale Character/factor vector of length \code{nrow(scale_emb)}.
#'   Names of the scales; used as the cosine column names.
#' @param make_plot Logical. If \code{TRUE} (default), \code{plot_dist} contains
#'   a distribution plot of all cosine coefficients; otherwise it is \code{NULL}.
#'
#' @return A list with:
#'   \describe{
#'     \item{cosim_mat}{Wide data frame, one row per item: \code{item_text},
#'       \code{scale}, one cosine column per scale (named by \code{factor_scale}),
#'       \code{best_target_factor}, \code{second_best_target_factor},
#'       \code{complexity}, \code{sparsity}, \code{within_sd}.}
#'     \item{plot_dist}{A ggplot distribution plot of the cosine coefficients,
#'       or \code{NULL} when \code{make_plot = FALSE}.}
#'   }
#'
#' @details
#' The cosine matrix is computed with \code{text2vec::sim2(method = "cosine",
#' norm = "l2")}. \code{complexity} is Hoffman's index and \code{sparsity} is
#' Hoyer's index of each item's profile across scales.
#'
#' @seealso \code{\link{cosim_xy}} for the general two-set version.
#'
#' @import text2vec
#' @importFrom dplyr bind_cols
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' item_emb  <- matrix(runif(60, -1, 1), nrow = 6, ncol = 10)
#' scale_emb <- matrix(runif(50, -1, 1), nrow = 5, ncol = 10)
#' res <- cosim_itens_scales(
#'   item_emb, scale_emb,
#'   item_text    = paste("item", 1:6),
#'   factor_itens = c("E", "N", "O", "A", "C", "O"),
#'   factor_scale = c("O", "C", "E", "A", "N")
#' )
#' head(res$cosim_mat)
#' res$plot_dist
#' }
#'
#' @export
cosim_itens_scales <- function(
    item_emb,
    scale_emb,
    item_text,
    factor_itens,
    factor_scale,
    make_plot = TRUE) {

  m1 <- as.matrix(item_emb)
  m2 <- as.matrix(scale_emb)

  cosim_mat <- as.data.frame(text2vec::sim2(
    x = m1, y = m2, method = "cosine", norm = "l2"
  ))
  colnames(cosim_mat) <- factor_scale

  # Best and second best scale per item
  best_two <- t(apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = function(x) {
    ord <- order(x, decreasing = TRUE)
    c(best_target_factor        = factor_scale[ord[1]],
      second_best_target_factor = if (length(ord) > 1) factor_scale[ord[2]] else NA_character_)
  }))
  cosim_mat$best_target_factor        <- best_two[, "best_target_factor"]
  cosim_mat$second_best_target_factor <- best_two[, "second_best_target_factor"]

  cosim_mat$complexity <- apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = hoffman_complexity)
  cosim_mat$sparsity   <- apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = hoyer_sparsity)
  cosim_mat$within_sd  <- apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = stats::sd)

  cosim_mat <- dplyr::bind_cols(
    item_text = item_text,
    scale     = factor_itens,
    cosim_mat
  )

  plot_dist <- NULL
  if (isTRUE(make_plot)) {
    plot_dist <- embeddcv_cosim_distribution_plot(unlist(cosim_mat[, factor_scale]))
  }

  list(cosim_mat = cosim_mat, plot_dist = plot_dist)
}


#' Cosine Similarities between Two Embedding Sets (or within one set)
#'
#' General cosine-similarity tool for any pair of embedding matrices: item-item,
#' scale-scale, scale-label, scale-definition, definition-definition, etc. When
#' \code{y} is \code{NULL}, \code{x} is compared with itself (a symmetric
#' within-set matrix). This is the engine used by the jingle-jangle analysis and
#' by other ad-hoc similarity comparisons.
#'
#' @param x Numeric matrix or data frame. Row embedding set (one row per unit).
#'   Row names are used as default ids.
#' @param y Numeric matrix or data frame, or \code{NULL}. Column embedding set
#'   with the same number of dimensions as \code{x}. If \code{NULL}, \code{x} is
#'   compared with itself.
#' @param x_labels,y_labels Optional character vectors of display labels for the
#'   rows of \code{x} and \code{y}. Default to the ids.
#' @param x_group,y_group Optional character/factor vectors of group labels for
#'   the rows of \code{x} and \code{y}. Default to the labels.
#' @param x_type,y_type Length-one labels describing each set (e.g. \code{"scale"},
#'   \code{"label"}). Copied into the long table and used to generate ids.
#' @param x_id,y_id Optional stable ids for the rows. Default to row names or
#'   generated ids; duplicates are made unique.
#' @param drop_self Logical. If \code{TRUE} and the comparison is symmetric
#'   (same ids), diagonal self-similarities are set to \code{NA}. Defaults to
#'   \code{TRUE} when \code{y} is \code{NULL}.
#' @param make_plot Logical. If \code{TRUE}, \code{plot} contains a distribution
#'   plot of the finite cosine coefficients. Default \code{FALSE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{matrix}{Raw numeric cosine matrix \[nrow(x) x nrow(y)\], with ids as
#'       dimnames.}
#'     \item{long}{Long pair table with columns \code{x_id, x_label, x_group,
#'       x_type, y_id, y_label, y_group, y_type, cosim}.}
#'     \item{plot}{A ggplot distribution plot, or \code{NULL} when
#'       \code{make_plot = FALSE}.}
#'   }
#'
#' @import text2vec
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' scale_emb <- matrix(rnorm(8 * 16), 8, dimnames = list(paste0("s", 1:8)))
#' label_emb <- matrix(rnorm(5 * 16), 5, dimnames = list(paste0("l", 1:5)))
#'
#' # cross-set (scale x label) — used by jingle-jangle relabeling
#' cross <- cosim_xy(scale_emb, label_emb, x_type = "scale", y_type = "label")
#' cross$matrix
#'
#' # within-set (scale x scale), diagonal dropped
#' ss <- cosim_xy(scale_emb, x_type = "scale")
#' ss$long
#' }
#'
#' @export
cosim_xy <- function(
    x,
    y         = NULL,
    x_labels  = NULL,
    y_labels  = NULL,
    x_group   = NULL,
    y_group   = NULL,
    x_type    = "x",
    y_type    = "y",
    x_id      = NULL,
    y_id      = NULL,
    drop_self = is.null(y),
    make_plot = FALSE) {

  xm <- embeddcv_as_embedding_matrix(x, "x")
  same_set <- is.null(y)
  ym <- if (same_set) xm else embeddcv_as_embedding_matrix(y, "y")

  if (ncol(xm) != ncol(ym)) {
    stop("`x` and `y` must have the same number of embedding dimensions.", call. = FALSE)
  }

  x_id <- embeddcv_vector_or_default(x_id, rownames(xm), nrow(xm), paste0(x_type, "_"))
  x_id <- make.unique(as.character(x_id), sep = "__")
  if (same_set) {
    y_id <- x_id
  } else {
    y_id <- embeddcv_vector_or_default(y_id, rownames(ym), nrow(ym), paste0(y_type, "_"))
    y_id <- make.unique(as.character(y_id), sep = "__")
  }
  rownames(xm) <- x_id
  rownames(ym) <- y_id

  x_labels <- as.character(embeddcv_vector_or_default(x_labels, NULL, nrow(xm), x_id))
  y_labels <- as.character(embeddcv_vector_or_default(y_labels, NULL, nrow(ym), y_id))
  x_group  <- as.character(embeddcv_vector_or_default(x_group, NULL, nrow(xm), x_labels))
  y_group  <- as.character(embeddcv_vector_or_default(y_group, NULL, nrow(ym), y_labels))

  mat <- as.matrix(text2vec::sim2(x = xm, y = ym, method = "cosine", norm = "l2"))
  dimnames(mat) <- list(x_id, y_id)

  if (isTRUE(drop_self) && nrow(mat) == ncol(mat) && identical(x_id, y_id)) {
    diag(mat) <- NA_real_
  }

  long <- data.frame(
    x_id  = rep(x_id, times = length(y_id)),
    y_id  = rep(y_id, each = length(x_id)),
    cosim = as.vector(mat),
    stringsAsFactors = FALSE
  )
  long$x_label <- x_labels[match(long$x_id, x_id)]
  long$x_group <- x_group[match(long$x_id, x_id)]
  long$x_type  <- x_type
  long$y_label <- y_labels[match(long$y_id, y_id)]
  long$y_group <- y_group[match(long$y_id, y_id)]
  long$y_type  <- y_type
  long <- long[c("x_id", "x_label", "x_group", "x_type",
                 "y_id", "y_label", "y_group", "y_type", "cosim")]

  plot <- if (isTRUE(make_plot)) embeddcv_cosim_distribution_plot(long$cosim) else NULL

  list(matrix = mat, long = long, plot = plot)
}


#' All pairwise cosine similarities among named embedding sets
#'
#' Convenience wrapper around \code{\link{cosim_xy}} producing all pairwise
#' comparisons among named embedding sets (e.g. item-item, item-scale,
#' scale-scale, scale-definition).
#'
#' @param embeddings Named list of numeric matrices/data frames, all with the
#'   same number of columns. Names become the set types (e.g. \code{"item"},
#'   \code{"scale"}, \code{"definition"}).
#' @param labels Optional named list of character vectors (names match
#'   \code{embeddings}); passed to \code{cosim_xy} as \code{x_labels}/\code{y_labels}.
#' @param groups Optional named list of character/factor vectors; passed as
#'   \code{x_group}/\code{y_group}.
#' @param include_same Logical. If \code{TRUE}, include within-set comparisons.
#' @param drop_self Logical. If \code{TRUE}, diagonal self-similarities in
#'   within-set comparisons are set to \code{NA}.
#' @param make_plot Logical passed to \code{cosim_xy}.
#'
#' @return A named list of \code{cosim_xy()} outputs (each with \code{matrix},
#'   \code{long}, \code{plot}), named \code{"<x_type>_<y_type>"}.
#'
#' @examples
#' \dontrun{
#' sims <- cosim_all_pairs(list(item = item_emb, scale = scale_emb))
#' names(sims)
#' sims$scale_scale$long
#' }
#' @export
cosim_all_pairs <- function(
    embeddings,
    labels       = NULL,
    groups       = NULL,
    include_same = TRUE,
    drop_self    = TRUE,
    make_plot    = FALSE) {

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

    same <- x_name == y_name
    out[[i]] <- cosim_xy(
      x        = embeddings[[x_name]],
      y        = if (same) NULL else embeddings[[y_name]],
      x_labels = labels[[x_name]],
      y_labels = if (same) NULL else labels[[y_name]],
      x_group  = groups[[x_name]],
      y_group  = if (same) NULL else groups[[y_name]],
      x_type   = x_name,
      y_type   = y_name,
      drop_self = isTRUE(drop_self) && same,
      make_plot = make_plot
    )
  }

  names(out) <- out_names
  out
}


# ── Internal helpers ─────────────────────────────────────────────────────────

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
      binwidth = 0.02, colour = "white", fill = "salmon", alpha = 0.85
    ) +
    ggplot2::geom_density(colour = "#c0392b", linewidth = 0.8, adjust = 1.2) +
    ggplot2::geom_vline(xintercept = pct, linetype = "dashed",
                        colour = "grey30", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = mean_val, linetype = "solid",
                        colour = "#2c3e50", linewidth = 0.8) +
    ggplot2::annotate("text", x = pct, y = Inf, label = names(pct),
                      angle = 90, vjust = -0.4, hjust = 1.1, size = 2.8,
                      colour = "grey30") +
    ggplot2::annotate("text", x = mean_val, y = Inf,
                      label = paste0("mean\n", round(mean_val, 3)),
                      vjust = -0.3, hjust = -0.15, size = 2.8, colour = "#2c3e50") +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(0.10)) +
    ggplot2::labs(
      title = "Distribution of Cosine Similarity Coefficients",
      subtitle = subtitle, x = "Cosine similarity", y = "Density"
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
#' \deqn{\frac{\sqrt{n} - \|x\|_1 / \|x\|_2}{\sqrt{n} - 1}.}
#' Non-finite values are removed first. Returns 0 for length 0/1 or zero L2 norm.
#'
#' @param x Numeric vector.
#' @return Numeric value between 0 and 1.
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
#' \deqn{\frac{(\sum_i x_i^2)^2}{\sum_i x_i^4}.}
#' Values near 1 indicate one element dominates; larger values indicate a more
#' spread profile. Non-finite values are removed first.
#'
#' @param x Numeric vector.
#' @return Numeric complexity value. Returns \code{NA} if the fourth-power sum is zero.
#' @export
hoffman_complexity <- function(x) {
  x <- x[is.finite(x)]
  sum_sq <- sum(x^2)
  sum_quad <- sum(x^4)
  if (sum_quad == 0) return(NA_real_)
  (sum_sq^2) / sum_quad
}
