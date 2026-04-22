
#' Dimensionality Reduction and Visualization of Item-Scale Cosine Similarities
#'
#' Takes a cosine similarity matrix (output of \code{cosim_itens_scales}) and runs
#' MDS, t-SNE, and UMAP on the item profiles across scales. Returns scatter plots
#' with item labels and a data frame of coordinates.
#'
#' @param cosim_mat Data frame. Output of \code{cosim_itens_scales}, containing
#'   columns \code{item_text}, scale columns, and optionally other columns.
#' @param factor_scale Character vector. Names of the scale columns to use as
#'   input features for dimensionality reduction.
#' @param colour_by Character string. Name of the column in \code{cosim_mat} to
#'   use for colouring points (e.g., \code{"scale"}). Default is \code{"scale"}.
#' @param n_dims Integer. Number of dimensions to compute (default 2).
#'   If 2: 2D ggplot scatter plots are returned.
#'   If 3: interactive 3D plotly plots are returned in addition to the 2D plots.
#'   If > 3: all dimensions are saved in the returned data frame but only 2D plots
#'   are produced (using the first two dimensions of each method).
#' @param tsne_perplexity Numeric. Perplexity parameter for t-SNE. Default is 30.
#'   Automatically clamped to a valid value for the sample size.
#' @param umap_n_neighbors Integer. Number of neighbours for UMAP. Default is 15.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#'
#' @return A list with:
#'   \describe{
#'     \item{coords}{A data frame with \code{item_text}, the \code{colour_by} column,
#'       and all computed dimensions for MDS (\code{mds_1}, \code{mds_2}, ...),
#'       t-SNE (\code{tsne_1}, \code{tsne_2}, ...), and UMAP (\code{umap_1}, \code{umap_2}, ...).}
#'     \item{plot_mds}{2D ggplot scatter of the first two MDS dimensions.}
#'     \item{plot_tsne}{2D ggplot scatter of the first two t-SNE dimensions.}
#'     \item{plot_umap}{2D ggplot scatter of the first two UMAP dimensions.}
#'     \item{plot_mds_3d}{Interactive plotly 3D scatter (only when \code{n_dims == 3}).}
#'     \item{plot_tsne_3d}{Interactive plotly 3D scatter (only when \code{n_dims == 3}).}
#'     \item{plot_umap_3d}{Interactive plotly 3D scatter (only when \code{n_dims == 3}).}
#'   }
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#'
#' @examples
#' \dontrun{
#' result_cosim <- cosim_itens_scales(item_emb, scale_emb, item_text, factor_itens, factor_scale)
#'
#' # 2D plots (default)
#' viz <- plot_cosim_dims(result_cosim, factor_scale = c("O","C","E","A","N"), colour_by = "scale")
#' viz$plot_mds
#'
#' # 3D interactive plots
#' viz3 <- plot_cosim_dims(result_cosim, factor_scale = c("O","C","E","A","N"),
#'                         colour_by = "scale", n_dims = 3)
#' viz3$plot_mds_3d
#'
#' # 5 dims saved, 2D plots returned
#' viz5 <- plot_cosim_dims(result_cosim, factor_scale = c("O","C","E","A","N"),
#'                         colour_by = "scale", n_dims = 5)
#' head(viz5$coords)
#' }
#'
#' @export
plot_cosim_dims <- function(
    cosim_mat,
    factor_scale,
    colour_by        = "scale",
    n_dims           = 2,
    tsne_perplexity  = 30,
    umap_n_neighbors = 15,
    seed             = 42) {

  set.seed(seed)

  # Extract feature matrix (items x scales)
  X <- as.matrix(cosim_mat[, factor_scale])

  # ── MDS ──────────────────────────────────────────────────────────────────────
  dist_mat <- dist(X)
  mds_fit  <- cmdscale(dist_mat, k = n_dims)
  if (n_dims == 1) mds_fit <- matrix(mds_fit, ncol = 1)
  mds_cols <- paste0("mds_", seq_len(n_dims))
  mds_df   <- as.data.frame(mds_fit)
  colnames(mds_df) <- mds_cols

  # ── t-SNE ────────────────────────────────────────────────────────────────────
  tsne_perplexity <- min(tsne_perplexity, floor((nrow(X) - 1) / 3))
  tsne_fit <- Rtsne::Rtsne(X, dims = n_dims, perplexity = tsne_perplexity,
                            check_duplicates = FALSE, verbose = FALSE)
  tsne_cols <- paste0("tsne_", seq_len(n_dims))
  tsne_df   <- as.data.frame(tsne_fit$Y)
  colnames(tsne_df) <- tsne_cols

  # ── UMAP ─────────────────────────────────────────────────────────────────────
  umap_config              <- umap::umap.defaults
  umap_config$n_components <- n_dims
  umap_config$n_neighbors  <- min(umap_n_neighbors, nrow(X) - 1)
  umap_config$random_state <- seed
  umap_fit  <- umap::umap(X, config = umap_config)
  umap_cols <- paste0("umap_", seq_len(n_dims))
  umap_df   <- as.data.frame(umap_fit$layout)
  colnames(umap_df) <- umap_cols

  # ── Combined coordinate data frame ───────────────────────────────────────────
  coords <- data.frame(
    item_text = cosim_mat$item_text,
    cosim_mat[, colour_by, drop = FALSE],
    mds_df,
    tsne_df,
    umap_df
  )

  # ── 2D ggplot helper ─────────────────────────────────────────────────────────
  make_plot_2d <- function(df, x_col, y_col, title) {
    ggplot2::ggplot(df, ggplot2::aes(
      x      = .data[[x_col]],
      y      = .data[[y_col]],
      colour = .data[[colour_by]],
      label  = item_text
    )) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggrepel::geom_text_repel(size = 2.5, max.overlaps = 20) +
      ggplot2::labs(title = title, x = x_col, y = y_col, colour = colour_by) +
      ggplot2::theme_minimal()
  }

  out <- list(
    coords    = coords,
    plot_mds  = make_plot_2d(coords, "mds_1",  "mds_2",  "MDS"),
    plot_tsne = make_plot_2d(coords, "tsne_1", "tsne_2", "t-SNE"),
    plot_umap = make_plot_2d(coords, "umap_1", "umap_2", "UMAP")
  )

  # ── 3D plotly plots (only when n_dims == 3) ──────────────────────────────────
  if (n_dims == 3) {
    make_plot_3d <- function(df, x_col, y_col, z_col, title) {
      plotly::plot_ly(
        data   = df,
        x      = stats::as.formula(paste0("~", x_col)),
        y      = stats::as.formula(paste0("~", y_col)),
        z      = stats::as.formula(paste0("~", z_col)),
        color  = stats::as.formula(paste0("~", colour_by)),
        text   = ~item_text,
        type   = "scatter3d",
        mode   = "markers+text",
        marker = list(size = 4)
      ) |>
        plotly::layout(title = title)
    }

    out$plot_mds_3d  <- make_plot_3d(coords, "mds_1",  "mds_2",  "mds_3",  "MDS 3D")
    out$plot_tsne_3d <- make_plot_3d(coords, "tsne_1", "tsne_2", "tsne_3", "t-SNE 3D")
    out$plot_umap_3d <- make_plot_3d(coords, "umap_1", "umap_2", "umap_3", "UMAP 3D")
  }

  out
}
