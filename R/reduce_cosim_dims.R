
#' Dimensionality Reduction and Optional Clustering of Item-Scale Cosine Similarities
#'
#' Runs MDS, t-SNE, and UMAP on item profiles across scales. Optionally also runs
#' k-means clustering on each method's coordinates and Exploratory Graph Analysis
#' (EGA) on item-item correlations, then computes Tucker's coefficient of congruence
#' between every cluster solution and an optional original item-factor labelling.
#'
#' @param cosim_mat Data frame. Output of \code{cosim_itens_scales$cosim_mat}.
#' @param factor_scale Character vector. Names of the scale columns to use as
#'   input features.
#' @param n_dims Integer. Number of dimensions to compute (default 2). If 3, 3D
#'   coordinates are stored. If > 3, all dimensions are stored.
#' @param n_clusters Integer or \code{NULL}. If non-\code{NULL}, runs k-means with
#'   \code{n_clusters} centres on each method's coordinates and adds the cluster
#'   assignments to \code{coords} as \code{mds_cluster}, \code{tsne_cluster},
#'   and \code{umap_cluster}. When \code{n_clusters} is set, EGA is also run on
#'   the item-item correlation matrix (the number of EGA communities is
#'   data-driven, not equal to \code{n_clusters}). Congruence among all groupings
#'   is then returned. Default \code{NULL} (no clustering).
#' @param item_factor Character or factor vector of length \code{nrow(cosim_mat)},
#'   or \code{NULL}. Original/expected factor placement for each item. When
#'   provided alongside \code{n_clusters}, it is included in the congruence matrix.
#' @param node_label Character. Name of the column in \code{cosim_mat} to use as
#'   node labels in the EGA network plot, or a character vector of length
#'   \code{nrow(cosim_mat)} supplying the labels directly. Default \code{NULL}
#'   uses the default row identifiers (typically item position numbers).
#' @param node_label_width Integer. If set, wraps long node labels at this many
#'   characters by inserting line breaks. Default \code{NULL} (no wrapping).
#' @param tsne_perplexity Numeric. t-SNE perplexity (default 30).
#' @param umap_n_neighbors Integer. UMAP number of neighbours (default 15).
#' @param seed Integer. Random seed for reproducibility (default 42).
#'
#' @return A list with:
#'   \describe{
#'     \item{coords}{A data frame with all non-scale columns from \code{cosim_mat}
#'       plus the MDS/t-SNE/UMAP coordinates. When \code{n_clusters} is set, also
#'       \code{mds_cluster}, \code{tsne_cluster}, \code{umap_cluster}, and
#'       \code{ega_cluster} (when EGA succeeds) with method-prefixed labels.}
#'     \item{congruence}{(only when \code{n_clusters} is set) Square matrix of
#'       Tucker's congruence between every group across every grouping.}
#'     \item{ega_loadings}{(only when \code{n_clusters} is set and EGA succeeds)
#'       A data frame of standardised network loadings per item per EGA community.}
#'     \item{ega_plot}{(only when \code{n_clusters} is set and EGA succeeds) The
#'       EGA network plot (a ggplot/patchwork object) showing items as nodes
#'       coloured by community and edges weighted by partial correlations.}
#'   }
#'
#' @details
#' EGA (Exploratory Graph Analysis) is run on the item-item correlation matrix
#' computed from the cosine similarity profiles across scales. Because the number
#' of "observations" for that correlation is the number of scales, results may be
#' unstable when \code{length(factor_scale)} is small.
#'
#' Tucker's congruence between two binary indicator vectors is the cosine
#' similarity — 0 (no overlap) to 1 (identical membership).
#'
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom psych dummy.code congruence
#' @importFrom stats kmeans cor cmdscale dist
#' @importFrom stringr str_wrap
#'
#' @examples
#' \dontrun{
#' result <- cosim_itens_scales(item_emb, scale_emb, item_text, factor_itens, factor_scale)
#'
#' red <- reduce_cosim_dims(result$cosim_mat, factor_scale,
#'                          n_clusters       = 5,
#'                          item_factor      = result$cosim_mat$scale,
#'                          node_label       = "item_text",
#'                          node_label_width = 20)
#' red$coords           # has *_cluster columns for mds, tsne, umap, ega
#' round(red$congruence, 2)
#' head(red$ega_loadings)
#' red$ega_plot         # network plot with wrapped item_text labels
#' }
#'
#' @export
reduce_cosim_dims <- function(
    cosim_mat,
    factor_scale,
    n_dims           = 2,
    n_clusters       = NULL,
    item_factor      = NULL,
    node_label       = NULL,
    node_label_width = NULL,
    tsne_perplexity  = 30,
    umap_n_neighbors = 15,
    seed             = 42) {

  set.seed(seed)

  X <- as.matrix(cosim_mat[, factor_scale])

  # ── MDS ──────────────────────────────────────────────────────────────────────
  mds_fit <- stats::cmdscale(d = stats::dist(X), k = n_dims)
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

  meta_cols <- setdiff(names(cosim_mat), factor_scale)
  coords <- data.frame(
    cosim_mat[, meta_cols, drop = FALSE],
    mds_df,
    tsne_df,
    umap_df
  )

  # ── No clustering requested ──────────────────────────────────────────────────
  if (is.null(n_clusters)) {
    return(list(coords = coords))
  }

  # ── k-means on each method's dimensions ──────────────────────────────────────
  set.seed(seed)
  mds_km  <- stats::kmeans(as.matrix(mds_df),  centers = n_clusters, nstart = 25)
  tsne_km <- stats::kmeans(as.matrix(tsne_df), centers = n_clusters, nstart = 25)
  umap_km <- stats::kmeans(as.matrix(umap_df), centers = n_clusters, nstart = 25)

  # Zero-pad cluster numbers so labels sort lexicographically (01, 02, ..., 10)
  pad <- function(x) formatC(as.integer(x), width = nchar(max(as.integer(x))),
                              flag = "0", format = "d")

  coords$mds_cluster  <- paste0("mds_",  pad(mds_km$cluster))
  coords$tsne_cluster <- paste0("tsne_", pad(tsne_km$cluster))
  coords$umap_cluster <- paste0("umap_", pad(umap_km$cluster))

  # ── EGA on item-item correlations of cosim profiles ─────────────────────────
  ega_loadings <- NULL
  ega_ok       <- FALSE

  ega_plot <- NULL

  if (requireNamespace("EGAnet", quietly = TRUE)) {
    # Each item's profile across factor_scale columns; correlate items pairwise.
    # X = cosim_mat[, factor_scale] → [n_items × n_scales]   (items in rows)
    # t(X) → [n_scales × n_items]                            (items in columns)
    # cor() correlates columns → item × item correlation matrix
    item_cor <- stats::cor(t(X))

    # Resolve node labels for the EGA network plot
    if (!is.null(node_label)) {
      labs <- if (length(node_label) == 1 && node_label %in% names(cosim_mat)) {
        as.character(cosim_mat[[node_label]])
      } else if (length(node_label) == nrow(cosim_mat)) {
        as.character(node_label)
      } else {
        warning("`node_label` must be a column name in cosim_mat or a vector of length nrow(cosim_mat); ignoring.")
        NULL
      }

      if (!is.null(labs)) {
        if (!is.null(node_label_width))
          labs <- stringr::str_wrap(labs, width = node_label_width)

        # Make labels unique (EGA requires unique node ids) by appending a
        # suffix to duplicates.
        if (anyDuplicated(labs))
          labs <- make.unique(labs, sep = "_")

        rownames(item_cor) <- colnames(item_cor) <- labs
      }
    }

    # Newer EGAnet: pass the correlation matrix via `data`; `corr` is now a
    # string naming the method, not a matrix. EGA detects symmetric input as
    # a correlation matrix automatically.
    ega_result <- tryCatch(
      EGAnet::EGA(
        data     = item_cor,
        n        = length(factor_scale),
        plot.EGA = FALSE,
        verbose  = FALSE
      ),
      error = function(e) { warning("EGA failed: ", e$message); NULL }
    )

    if (!is.null(ega_result)) {
      coords$ega_cluster <- paste0("ega_", pad(ega_result$wc))

      ega_loadings <- tryCatch(
        as.data.frame(EGAnet::net.loads(ega_result)$std),
        error = function(e) { warning("EGA loadings failed: ", e$message); NULL }
      )

      # Network plot — try the modern S3 plot method first, fall back to
      # plot.EGA() if needed.
      ega_plot <- tryCatch(
        plot(ega_result),
        error = function(e) {
          tryCatch(EGAnet::plot.EGA(ega_result),
                   error = function(e2) {
                     warning("EGA plot failed: ", e2$message); NULL
                   })
        }
      )

      ega_ok <- TRUE
    }
  } else {
    warning("EGAnet package is not installed; skipping EGA. ",
            "Install with install.packages('EGAnet').")
  }

  # ── Build dummy matrices and compute congruence ─────────────────────────────
  dummies_list <- list()

  if (!is.null(item_factor)) {
    factor_labels <- paste0("factor_", as.character(item_factor))
    dummies_list$factor <- psych::dummy.code(factor_labels)
  }
  dummies_list$mds  <- psych::dummy.code(coords$mds_cluster)
  dummies_list$tsne <- psych::dummy.code(coords$tsne_cluster)
  dummies_list$umap <- psych::dummy.code(coords$umap_cluster)
  if (ega_ok) dummies_list$ega <- psych::dummy.code(coords$ega_cluster)

  all_dummies        <- do.call(cbind, dummies_list)
  congruence_matrix  <- psych::congruence(all_dummies)

  out <- list(
    coords     = coords,
    congruence = congruence_matrix
  )
  if (ega_ok) {
    out$ega_loadings <- ega_loadings
    out$ega_plot     <- ega_plot
  }

  out
}
