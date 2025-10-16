#' Row-wise cosine similarities between two matrices
#'
#' @description
#' Computes pairwise similarities between rows of `x` and rows of `y`
#' using \code{text2vec::sim2()}. The first column contains labels for
#' rows of `x`; columns are labeled by `y` row labels.
#'
#' @param x A numeric matrix/data.frame (rows = items to compare).
#' @param y A numeric matrix/data.frame (rows = targets to compare against).
#' @param x_labels Optional character vector of length \code{nrow(x)} to label
#'   rows of \code{x}. If \code{NULL}, uses \code{rownames(x)} or sequence.
#' @param y_labels Optional character vector of length \code{nrow(y)} to label
#'   columns (i.e., rows of \code{y}). If \code{NULL}, uses \code{rownames(y)} or sequence.
#' @param id_col Name of the identifier column for \code{x} labels. Default \code{"id"}.
#' @param norm01 Logical; if \code{TRUE}, min–max scales the similarity matrix to [0,1].
#' @param ... Additional arguments passed to \code{text2vec::sim2()}
#'   (e.g., \code{method = "cosine"}, \code{norm = "l2"}). Defaults are those of \code{sim2()}.
#'
#' @return A \code{data.frame}: first column = \code{id_col} (x labels),
#'   remaining columns = similarities to each row of \code{y} (named by \code{y_labels}).
#'
#' @details
#' Non-numeric columns are dropped. If coercion produces \code{NA}s, an error is thrown.
#'
#' @examples
#' \dontrun{
#' library(text2vec)
#' set.seed(1)
#' x <- matrix(rnorm(9), 3, 3)
#' y <- matrix(rnorm(12), 4, 3)
#' cosine_xy(x, y,
#'           x_labels = paste0("x", 1:3),
#'           y_labels = paste0("y", 1:4),
#'           method = "cosine", norm = "l2")
#' }
#'
#' @importFrom text2vec sim2
#' @export
cosine_xy <- function(
  x,
  y,
  x_labels = NULL,
  y_labels = NULL,
  id_col   = "id",
  norm01   = FALSE,
  ...
) {
 
 # Coerce to numeric matrices
 m_x <- as.matrix(x)
 m_y <- as.matrix(y)
 storage.mode(m_x) <- "double"
 storage.mode(m_y) <- "double"
 
 
 if (!all(is.finite(m_x))) stop("Non-finite values in 'x' after coercion.")
 if (!all(is.finite(m_y))) stop("Non-finite values in 'y' after coercion.")
 if (ncol(m_x) != ncol(m_y)) {
  stop(sprintf("Column mismatch: ncol(x)=%d, ncol(y)=%d.", ncol(m_x), ncol(m_y)))
 }
 
 # Labels
 if (is.null(x_labels)) {
  x_labels <- rownames(m_x)
  if (is.null(x_labels)) x_labels <- as.character(seq_len(nrow(m_x)))
 } else if (length(x_labels) != nrow(m_x)) {
  stop("'x_labels' length must equal nrow(x).")
 }
 
 if (is.null(y_labels)) {
  y_labels <- rownames(m_y)
  if (is.null(y_labels)) y_labels <- as.character(seq_len(nrow(m_y)))
 } else if (length(y_labels) != nrow(m_y)) {
  stop("'y_labels' length must equal nrow(y).")
 }
 
 # Similarity (defaults to sim2 defaults unless overridden in ...)
 sim_mat <- text2vec::sim2(x = m_x, y = m_y, ...)
 
 # Optional [0,1] scaling
 if (isTRUE(norm01)) {
  rng <- range(sim_mat, na.rm = TRUE)
  if (diff(rng) > 0) {
   sim_mat <- (sim_mat - rng[1]) / (rng[2] - rng[1])
  } else {
   sim_mat[] <- 0
  }
 }
 
 sim_df <- as.data.frame(sim_mat, check.names = FALSE, stringsAsFactors = FALSE)
 colnames(sim_df) <- y_labels
 
 out <- cbind(setNames(data.frame(x_labels, check.names = FALSE), id_col), sim_df)
 rownames(out) <- NULL
 out
}
