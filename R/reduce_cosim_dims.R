
#' Dimensionality Reduction of Item-Scale Cosine Similarities
#'
#' Runs MDS, t-SNE, and UMAP on the item profiles across scales and returns
#' a data frame of coordinates. Use \code{plot_cosim_dims()} to visualize the result.
#'
#' @param cosim_mat Data frame. Output of \code{cosim_itens_scales$cosim_mat}.
#' @param factor_scale Character vector. Names of the scale columns to use as
#'   input features.
#' @param n_dims Integer. Number of dimensions to compute (default 2).
#'   If 3, three-dimensional coordinates are also stored. If > 3, all dimensions
#'   are stored; \code{plot_cosim_dims()} will always plot the first two.
#' @param tsne_perplexity Numeric. t-SNE perplexity (default 30). Automatically
#'   clamped to a valid value for the sample size.
#' @param umap_n_neighbors Integer. UMAP number of neighbours (default 15).
#' @param seed Integer. Random seed for reproducibility (default 42).
#'
#' @return A data frame with one row per item containing all non-scale columns
#'   from \code{cosim_mat} (e.g., \code{item_text}, \code{scale},
#'   \code{best_target_factor}, etc.) plus \code{mds_1}/\code{mds_2}/...,
#'   \code{tsne_1}/\code{tsne_2}/..., and \code{umap_1}/\code{umap_2}/...
#'   columns.
#'
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#'
#' @examples
#' \dontrun{
#' result  <- cosim_itens_scales(item_emb, scale_emb, item_text, factor_itens, factor_scale)
#' coords  <- reduce_cosim_dims(result$cosim_mat, factor_scale)
#' plots   <- plot_cosim_dims(coords, colour_by = "scale")
#' plots$plot_mds
#' }
#'
#' @export
reduce_cosim_dims <- function(
    cosim_mat,
    factor_scale,
    n_dims           = 2,
    tsne_perplexity  = 30,
    umap_n_neighbors = 15,
    seed             = 42) {

  set.seed(seed)

  X <- as.matrix(cosim_mat[, factor_scale])

  # ── MDS ──────────────────────────────────────────────────────────────────────
  mds_fit <- cmdscale(d = dist(X), k = n_dims)
  if (n_dims == 1) mds_fit <- matrix(mds_fit, ncol = 1)
  mds_df  <- as.data.frame(mds_fit)
  colnames(mds_df) <- paste0("mds_", seq_len(n_dims))

  # ── t-SNE ────────────────────────────────────────────────────────────────────
  perp     <- min(tsne_perplexity, floor((nrow(X) - 1) / 3))
  tsne_fit <- Rtsne::Rtsne(X, dims = n_dims, perplexity = perp,
                            check_duplicates = FALSE, verbose = FALSE)
  tsne_df  <- as.data.frame(tsne_fit$Y)
  colnames(tsne_df) <- paste0("tsne_", seq_len(n_dims))

  # ── UMAP ─────────────────────────────────────────────────────────────────────
  cfg              <- umap::umap.defaults
  cfg$n_components <- n_dims
  cfg$n_neighbors  <- min(umap_n_neighbors, nrow(X) - 1)
  cfg$random_state <- seed
  umap_fit <- umap::umap(X, config = cfg)
  umap_df  <- as.data.frame(umap_fit$layout)
  colnames(umap_df) <- paste0("umap_", seq_len(n_dims))

  # Keep all non-scale columns from cosim_mat so colour_by options are available
  meta_cols <- setdiff(names(cosim_mat), factor_scale)

  data.frame(
    cosim_mat[, meta_cols, drop = FALSE],
    mds_df,
    tsne_df,
    umap_df
  )
}
