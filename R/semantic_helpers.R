#' Compute a cosine similarity matrix
#'
#' Computes all pairwise cosine similarities among rows of an embedding matrix.
#' Rows are observations such as items, scales, or definitions; columns are
#' embedding dimensions.
#'
#' @details
#' For two embedding vectors \eqn{x_i} and \eqn{x_j}, cosine similarity is
#'
#' \deqn{\cos(x_i, x_j) =
#' \frac{x_i^\top x_j}{\|x_i\|_2 \|x_j\|_2}.}
#'
#' The implementation L2-normalizes each row and then uses `tcrossprod()` to
#' compute the full symmetric similarity matrix efficiently. Zero-norm rows are
#' converted to `NA` after normalization.
#'
#' @param x Numeric matrix or data frame of embeddings. Rows are observations
#'   (for example items, scales, or definitions) and columns are embedding
#'   dimensions. All columns must be numeric; row names, when present, are
#'   copied to the row and column names of the returned similarity matrix.
#'
#' @return Symmetric numeric cosine similarity matrix with the same row names as
#'   `x` on both rows and columns.
#'
#' @examples
#' x <- matrix(c(1, 0, 0, 1, 1, 1), nrow = 3, byrow = TRUE)
#' rownames(x) <- c("a", "b", "c")
#' cosine_matrix(x)
#' @export
cosine_matrix <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  rn <- rownames(x)
  norms <- sqrt(rowSums(x * x))
  norms[norms == 0] <- NA_real_
  x <- x / norms
  out <- tcrossprod(x)
  rownames(out) <- colnames(out) <- rn
  out
}

#' Rescale similarities to a target interval
#'
#' Rescales a numeric vector, matrix, or array linearly to a target interval.
#' This is useful for reproducing the relabeling workflow from Wulff & Mata
#' (2025), where scale-scale cosine similarities are min-max rescaled before
#' clustering.
#'
#' @details
#' For input value \eqn{x}, input range \eqn{[a,b]}, and target range
#' \eqn{[c,d]}, the transformed value is
#'
#' \deqn{x' = \frac{x-a}{b-a}(d-c)+c.}
#'
#' When `from = NULL`, \eqn{a} and \eqn{b} are the finite minimum and maximum of
#' `x`. Missing and infinite values are not used to determine the range, but are
#' preserved in the output.
#'
#' @param x Numeric vector, matrix, or array to rescale. Existing dimensions,
#'   names, and dimnames are preserved in the returned object.
#' @param to Numeric vector of length 2 with the target range, for example
#'   `c(0, 1)`. The first value is the new minimum and the second value is the
#'   new maximum.
#' @param from Optional numeric vector of length 2 with the input range. When
#'   `NULL`, the finite range of `x` is used. Supply this when multiple objects
#'   must be placed on the same scale.
#'
#' @return Object with the same dimensions and dimnames as `x`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
rescale_similarity <- function(x, to = c(0, 1), from = NULL) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  if (length(to) != 2 || any(!is.finite(to))) {
    stop("`to` must be a finite numeric vector of length 2.", call. = FALSE)
  }

  if (is.null(from)) {
    from <- range(x, finite = TRUE)
  }
  if (length(from) != 2 || any(!is.finite(from))) {
    stop("`from` must be a finite numeric vector of length 2.", call. = FALSE)
  }
  if (from[1] == from[2]) {
    stop("`from` cannot have zero width.", call. = FALSE)
  }

  (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
}

#' Average item embeddings by scale
#'
#' Aggregates item-level embeddings into scale-level embeddings by taking the
#' column-wise mean of all item vectors that belong to the same scale.
#'
#' @details
#' For scale \eqn{s} with item set \eqn{I_s}, the scale embedding is
#'
#' \deqn{\bar{x}_s = \frac{1}{|I_s|}\sum_{i \in I_s} x_i.}
#'
#' Missing values are removed dimension-wise with `na.rm = TRUE`.
#'
#' @param emb Numeric matrix or data frame of item embeddings. Rows are items
#'   and columns are embedding dimensions. This can be the output of
#'   `get_embeddings()` after converting to a numeric matrix.
#' @param scale_id Character or factor vector assigning each item row to a
#'   scale. Must have length `nrow(emb)`. Unique values become row names in the
#'   returned scale embedding matrix.
#'
#' @return Numeric matrix with one row per unique scale id and one column per
#'   embedding dimension.
#' @export
get_scale_embeddings <- function(emb, scale_id) {
  emb <- as.matrix(emb)
  scale_id <- as.character(scale_id)
  scale_ids <- unique(scale_id)
  out <- matrix(
    NA_real_,
    nrow = length(scale_ids),
    ncol = ncol(emb),
    dimnames = list(scale_ids, colnames(emb))
  )

  for (i in seq_along(scale_ids)) {
    out[i, ] <- colMeans(emb[scale_id == scale_ids[i], , drop = FALSE], na.rm = TRUE)
  }

  out
}

#' Average item-item cosine similarities to scale-scale similarities
#'
#' Computes a scale-scale similarity matrix by averaging item-item similarities
#' for every pair of scales.
#'
#' @details
#' Let \eqn{I_a} be items from scale \eqn{a} and \eqn{I_b} be items from scale
#' \eqn{b}. The scale similarity is
#'
#' \deqn{S_{ab} = \operatorname{mean}\{C_{ij}: i \in I_a, j \in I_b\}.}
#'
#' When \eqn{a=b}, values greater than `self_cutoff` are excluded before taking
#' the mean. This removes diagonal item self-similarities, which are typically
#' equal or extremely close to 1 and would otherwise inflate within-scale
#' similarity.
#'
#' @param cos Numeric square item-item cosine similarity matrix. Rows and
#'   columns must represent the same items in the same order, such as the output
#'   of `cosine_matrix(item_embeddings)`.
#' @param scale_id Character or factor vector assigning each item row/column to
#'   a scale. Must have length `nrow(cos)` and must be ordered in the same item
#'   order as the rows and columns of `cos`.
#' @param self_cutoff Numeric scalar. Values greater than this cutoff are
#'   ignored when averaging within the same scale, typically to remove diagonal
#'   self-similarities near 1.
#'
#' @return Symmetric scale-scale similarity matrix.
#' @export
get_item_scale_cos <- function(cos, scale_id, self_cutoff = .999999) {
  cos <- as.matrix(cos)
  scale_id <- as.character(scale_id)
  scales_uni <- unique(scale_id)
  out <- matrix(
    1,
    nrow = length(scales_uni),
    ncol = length(scales_uni),
    dimnames = list(scales_uni, scales_uni)
  )

  for (i in seq_along(scales_uni)) {
    for (j in i:length(scales_uni)) {
      sel_i <- scale_id == scales_uni[i]
      sel_j <- scale_id == scales_uni[j]
      sub <- cos[sel_i, sel_j, drop = FALSE]
      out[i, j] <- out[j, i] <- mean(sub[sub < self_cutoff], na.rm = TRUE)
    }
  }

  out
}

#' Shorten text labels with ellipses
#'
#' Truncates long labels for dense plots while keeping short labels unchanged.
#' It is used internally by jingle-jangle plots so long construct names fit in
#' side annotations; it is not intended for semantic matching or data cleaning.
#'
#' @param x Character vector of labels to display.
#' @param n Maximum number of visible characters before adding `"..."`.
#'
#' @return Character vector where values longer than `n` characters are replaced
#'   by `substr(x, 1, n)` followed by `"..."`.
#'
#' @examples
#' short_text("interpersonal_sensitivity", n = 12)
#' short_text(c("brief", "very_long_construct_name"), n = 10)
#' @export
short_text <- function(x, n = 18) {
  x <- as.character(x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

#' Mix two colors
#'
#' Lightweight local implementation adapted from the color-mixing behavior of
#' `memnet::cmix()` (Wulff, 2019). It linearly interpolates RGB channels between
#' an input color and a second color, often `"white"` or `"black"`, to create
#' lighter or darker variants.
#'
#' @details
#' For RGB vectors \eqn{c_1} and \eqn{c_2}, the mixed color is
#'
#' \deqn{c_{mix} = (1 - amount)c_1 + amount c_2.}
#'
#' Thus `amount = 0` returns `col`, `amount = 1` returns `with`, and
#' intermediate values blend the two.
#'
#' @param col Color vector accepted by `grDevices::col2rgb()`.
#' @param with Color to mix into `col`.
#' @param amount Numeric value between 0 and 1 giving the proportion of `with`
#'   in the mixture.
#'
#' @return Hex color vector.
#'
#' @references
#' Wulff, D. U. (2019). memnet: Network Tools for Memory Research. R package.
#' https://www.rdocumentation.org/packages/memnet/versions/0.1.0
#' @export
mix_col <- function(col, with = "white", amount = .5) {
  c1 <- grDevices::col2rgb(col) / 255
  c2 <- grDevices::col2rgb(with) / 255

  if (ncol(c1) == 1 && length(col) > 1) {
    c1 <- matrix(rep(c1, length(col)), nrow = 3)
  }
  if (ncol(c2) == 1 && length(col) > 1) {
    c2 <- matrix(rep(c2, length(col)), nrow = 3)
  }

  mixed <- (1 - amount) * c1 + amount * c2
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ])
}

capitalize_first <- function(x) {
  x <- as.character(x)
  paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}
