#' Relabel clusters by maximum-weight bipartite matching
#'
#' Assigns one semantic label to each cluster in a clustering solution. The
#' assignment follows the maximum-weight matching strategy used by Wulff &
#' Mata (2025): each cluster is matched to the label whose embeddings are, in
#' aggregate, most similar to the scales inside that cluster, with the
#' additional constraint that each label can be used by only one cluster.
#'
#' @details
#' Let \eqn{C_k} be cluster \eqn{k}, let \eqn{L_\ell} be candidate label
#' \eqn{\ell}, and let \eqn{M_{i\ell}} be the scale-label similarity between
#' scale \eqn{i} and label \eqn{\ell}. For each cluster-label pair the function
#' computes the aggregate semantic fit
#'
#' \deqn{W_{k\ell} = \sum_{i \in C_k} M_{i\ell}.}
#'
#' The matrix \eqn{W} is then represented as a complete weighted bipartite
#' graph: one partition contains clusters and the other contains candidate
#' labels. The matching is the binary assignment matrix \eqn{X_{k\ell}} that
#' maximizes
#'
#' \deqn{\sum_k \sum_\ell W_{k\ell}X_{k\ell}}
#'
#' subject to each cluster receiving one label
#'
#' \deqn{\sum_\ell X_{k\ell} = 1}
#'
#' and each label being used at most once
#'
#' \deqn{\sum_k X_{k\ell} \le 1.}
#'
#' `igraph::max_bipartite_match()` solves this maximum-weight matching problem
#' on the weighted graph. The matched cluster labels are then expanded back to a
#' scale-level vector, so every scale inherits the label selected for its
#' cluster. This follows the relabeling procedure used by Wulff & Mata (2025):
#' cluster-label weights are computed with column sums of the scale-label
#' similarity matrix and labels are assigned by maximum bipartite matching.
#'
#' This means the selected label for a cluster is not necessarily the best label
#' for every individual scale in the cluster. It is the label that maximizes the
#' total semantic fit of the cluster while respecting the global one-label-per-
#' cluster matching.
#'
#' @param solution Named cluster membership vector. It may be numeric,
#'   character, or factor-like, with one element per scale. `names(solution)`
#'   must be scale ids and must occur in `rownames(scale_label_cross)`. The
#'   values identify cluster membership; scales with the same value are treated
#'   as one cluster. This is usually one membership vector from
#'   `make_relabeling_clustering_solutions()`.
#' @param scale_label_cross Numeric matrix with scales in rows and candidate
#'   labels/definitions in columns. Row names must be scale ids; column names
#'   must be label ids. Entries are usually cosine similarities from
#'   `cosim_itens_scales(..., x_type = "scale", y_type = "definition")$cosim_matrix`.
#'   Row names must use the same scale ids as `names(solution)`. If the full
#'   `cosim_itens_scales()` output list is passed, its `$cosim_matrix` element
#'   is used automatically.
#'
#' @return A named character vector with one entry per scale. Names are scale
#'   ids and values are the matched label ids assigned to the scale's cluster.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' scale_label_cross <- matrix(
#'   c(.90, .10, .80, .20, .15, .85),
#'   nrow = 3,
#'   byrow = TRUE,
#'   dimnames = list(c("s1", "s2", "s3"), c("label_a", "label_b"))
#' )
#' solution <- c(s1 = 1, s2 = 1, s3 = 2)
#' relabel_solution(solution, scale_label_cross)
#' @export
relabel_solution <- function(solution, scale_label_cross) {
  if (is.null(names(solution))) {
    stop("`solution` must be a named vector with scale ids in names(solution).", call. = FALSE)
  }

  # Friendly handling when the full cosim_itens_scales() output is passed
  if (is.list(scale_label_cross) && !is.data.frame(scale_label_cross) &&
      !is.null(scale_label_cross$cosim_matrix)) {
    scale_label_cross <- scale_label_cross$cosim_matrix
  }

  scale_label_cross <- as.matrix(scale_label_cross)

  if (is.null(rownames(scale_label_cross)) || is.null(colnames(scale_label_cross))) {
    stop(
      "`scale_label_cross` must have row names (scale ids) and column names ",
      "(label ids). Use cosim_itens_scales(...)$cosim_matrix, which sets both.",
      call. = FALSE
    )
  }

  missing_ids <- setdiff(names(solution), rownames(scale_label_cross))
  if (length(missing_ids) == length(solution)) {
    stop(
      "None of names(solution) match rownames(scale_label_cross). ",
      "The clustering solutions and the scale-label cross matrix must use the ",
      "same scale ids - typically both derive from the same scale embedding ",
      "matrix. First solution ids: ",
      paste(utils::head(names(solution), 2), collapse = " | "),
      " ... vs cross rownames: ",
      paste(utils::head(rownames(scale_label_cross), 2), collapse = " | "),
      call. = FALSE
    )
  }
  if (length(missing_ids) > 0) {
    stop(
      length(missing_ids), " scale id(s) in names(solution) are missing from ",
      "rownames(scale_label_cross): ",
      paste(utils::head(missing_ids, 3), collapse = " | "),
      if (length(missing_ids) > 3) " ..." else "",
      call. = FALSE
    )
  }
  extra_ids <- setdiff(rownames(scale_label_cross), names(solution))
  if (length(extra_ids) > 0) {
    stop(
      length(extra_ids), " row(s) of scale_label_cross have no membership in ",
      "`solution`: ",
      paste(utils::head(extra_ids, 3), collapse = " | "),
      if (length(extra_ids) > 3) " ..." else "",
      call. = FALSE
    )
  }

  solution <- solution[rownames(scale_label_cross)]
  cls <- sort(unique(solution))

  if (length(cls) > ncol(scale_label_cross)) {
    stop(
      "Solution has ", length(cls), " clusters but scale_label_cross offers ",
      "only ", ncol(scale_label_cross), " candidate labels; one-label-per-",
      "cluster matching is impossible. Restrict k_seq to at most the number ",
      "of labels.",
      call. = FALSE
    )
  }

  # Internal cluster vertex ids are prefixed so they can never collide with a
  # label id that happens to look like a cluster number (igraph matches
  # vertices by name).
  cluster_ids <- paste0("cl__", seq_along(cls))

  weights <- matrix(
    NA_real_,
    nrow = length(cls),
    ncol = ncol(scale_label_cross),
    dimnames = list(cluster_ids, colnames(scale_label_cross))
  )

  for (i in seq_along(cls)) {
    weights[i, ] <- colSums(scale_label_cross[solution == cls[i], , drop = FALSE], na.rm = TRUE)
  }

  edge_list <- expand.grid(
    clusters = rownames(weights),
    constructs = colnames(weights),
    stringsAsFactors = FALSE
  )
  edge_list$weight <- weights[cbind(edge_list$clusters, edge_list$constructs)]

  # Shift weights to be strictly positive: max_bipartite_match would rather
  # leave a cluster unmatched than use a negative-weight edge, and a constant
  # shift does not change which perfect matching is optimal.
  min_w <- min(edge_list$weight, na.rm = TRUE)
  if (min_w <= 0) {
    edge_list$weight <- edge_list$weight - min_w + 1e-6
  }

  g <- igraph::make_empty_graph(directed = FALSE)
  g <- igraph::add_vertices(
    g,
    nv = nrow(weights),
    attr = list(name = rownames(weights), type = rep(TRUE, nrow(weights)))
  )
  g <- igraph::add_vertices(
    g,
    nv = ncol(weights),
    attr = list(name = colnames(weights), type = rep(FALSE, ncol(weights)))
  )
  g <- igraph::add_edges(
    g,
    c(t(edge_list[, c("clusters", "constructs")])),
    attr = list(weight = edge_list$weight)
  )

  matched <- igraph::max_bipartite_match(g)
  cluster_labels <- matched$matching[cluster_ids]

  if (anyNA(cluster_labels)) {
    stop(
      "Bipartite matching failed to assign a label to every cluster (",
      sum(is.na(cluster_labels)), " unmatched). This should not happen when ",
      "the number of clusters does not exceed the number of labels.",
      call. = FALSE
    )
  }

  # Map each scale to its cluster's matched label via the cluster's position
  # in `cls` (robust to non-consecutive or character cluster codes).
  out <- unname(cluster_labels)[match(solution, cls)]
  names(out) <- names(solution)
  out
}

#' Score jingle and jangle fallacies in a relabeled solution
#'
#' Counts how many scale pairs become semantically inconsistent after a
#' relabeling solution is applied. This is the scoring step used to compare
#' original and relabeled taxonomies in the workflow described by Wulff &
#' Mata (2025).
#'
#' @details
#' For every unordered scale pair \eqn{(i,j)}, the function compares:
#'
#' \deqn{S_{ij} = \textrm{similarity between scale } i \textrm{ and scale } j}
#'
#' and
#'
#' \deqn{L_{ij} = \textrm{similarity between the labels assigned to } i
#' \textrm{ and } j.}
#'
#' Using thresholds \eqn{s_{low}}, \eqn{s_{high}}, \eqn{l_{low}}, and
#' \eqn{l_{high}}, pairs are classified as:
#'
#' \deqn{\textrm{jingle} \iff S_{ij} < s_{low} \ \textrm{and}\ L_{ij} > l_{high}}
#'
#' \deqn{\textrm{jangle} \iff S_{ij} > s_{high} \ \textrm{and}\ L_{ij} < l_{low}.}
#'
#' The total fallacy score used for solution selection is
#'
#' \deqn{
#' F = \sum_{i < j} I(S_{ij} < s_{low}, L_{ij} > l_{high}) +
#'     \sum_{i < j} I(S_{ij} > s_{high}, L_{ij} < l_{low}),
#' }
#'
#' which is returned as `fallacy_sum = jingle + jangle`. All remaining pairs are
#' counted as `good`. Only the upper triangle of the pair matrix is used, so
#' each unordered scale pair is scored once.
#'
#' @param solution Named character vector mapping scale ids to assigned label
#'   ids. `names(solution)` are scale ids and values are label ids. This is
#'   usually the output of `relabel_solution()` or an original scale-to-label
#'   mapping.
#' @param scale_cos Numeric square scale-scale similarity matrix. Row and column
#'   names must include every scale id in `names(solution)`, in any order.
#' @param label_cos Numeric square label-label similarity matrix. Row and column
#'   names must include every assigned label id in `solution`, in any order.
#' @param thresholds Named numeric vector with `scale_low`, `label_low`,
#'   `scale_high`, and `label_high`, typically returned by
#'   `jingle_jangle_thresholds()`.
#'
#' @return Named integer vector with four counts: `good`, `jangle`, `jingle`,
#'   and `fallacy_sum`, where `fallacy_sum = jangle + jingle`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' scale_cos <- matrix(c(1, .20, .80, .20, 1, .30, .80, .30, 1), 3)
#' rownames(scale_cos) <- colnames(scale_cos) <- c("s1", "s2", "s3")
#' label_cos <- matrix(c(1, .90, .10, .90, 1, .20, .10, .20, 1), 3)
#' rownames(label_cos) <- colnames(label_cos) <- c("l1", "l2", "l3")
#' solution <- c(s1 = "l1", s2 = "l2", s3 = "l3")
#' thresholds <- c(scale_low = .40, label_low = .40,
#'                 scale_high = .70, label_high = .70)
#' score_label_solution(solution, scale_cos, label_cos, thresholds)
#' @export
score_label_solution <- function(solution, scale_cos, label_cos, thresholds) {
  n <- length(solution)
  idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  scale_i <- names(solution)[idx[, 1]]
  scale_j <- names(solution)[idx[, 2]]
  label_i <- solution[idx[, 1]]
  label_j <- solution[idx[, 2]]

  scale_pair_cos <- scale_cos[cbind(scale_i, scale_j)]
  label_pair_cos <- label_cos[cbind(label_i, label_j)]

  type <- ifelse(
    scale_pair_cos < thresholds[["scale_low"]] &
      label_pair_cos > thresholds[["label_high"]],
    "Jingle fallacy",
    ifelse(
      scale_pair_cos > thresholds[["scale_high"]] &
        label_pair_cos < thresholds[["label_low"]],
      "Jangle fallacy",
      "Good"
    )
  )

  counts <- table(factor(type, levels = c("Good", "Jangle fallacy", "Jingle fallacy")))

  c(
    good = as.integer(counts["Good"]),
    jangle = as.integer(counts["Jangle fallacy"]),
    jingle = as.integer(counts["Jingle fallacy"]),
    fallacy_sum = as.integer(counts["Jangle fallacy"] + counts["Jingle fallacy"])
  )
}

#' Relabel and score one clustering solution
#'
#' Convenience wrapper that first calls `relabel_solution()` and then evaluates
#' the resulting label assignment with `score_label_solution()`. This implements
#' the candidate-taxonomy scoring step used by Wulff & Mata (2025): a cluster
#' solution is relabeled by maximum-weight bipartite matching between clusters
#' and labels, and the resulting label assignment is scored for jingle
#' fallacies (similar labels assigned to dissimilar scales) and jangle
#' fallacies (dissimilar labels assigned to similar scales). Use this when you
#' already have one candidate taxonomy and want both its matched labels and its
#' jingle/jangle counts.
#'
#' @param clustering_solution Named cluster membership vector, usually one
#'   element of the list returned by `make_relabeling_clustering_solutions()`.
#'   Names are scale ids; values identify cluster membership.
#' @param scale_label_cross Numeric scale-by-label similarity matrix used for
#'   maximum-weight relabeling. Rows must include the scale ids in
#'   `names(clustering_solution)` and columns must be candidate label ids.
#' @param scale_cos Numeric square scale-scale similarity matrix used to score
#'   jingle and jangle fallacies. Row and column names must include the scale
#'   ids in `names(clustering_solution)`.
#' @param label_cos Numeric square label-label similarity matrix used to score
#'   assigned labels. Row and column names must include the candidate labels in
#'   `colnames(scale_label_cross)`.
#' @param thresholds Named numeric vector from `jingle_jangle_thresholds()` with
#'   `scale_low`, `label_low`, `scale_high`, and `label_high`.
#'
#' @return A list with two components:
#' \describe{
#'   \item{labels}{Named character vector returned by `relabel_solution()`.}
#'   \item{scores}{Named integer vector returned by `score_label_solution()`.}
#' }
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
score_clustering_solution <- function(
    clustering_solution,
    scale_label_cross,
    scale_cos,
    label_cos,
    thresholds) {

  labels <- relabel_solution(clustering_solution, scale_label_cross)
  scores <- score_label_solution(
    labels,
    scale_cos = scale_cos,
    label_cos = label_cos,
    thresholds = thresholds
  )

  list(labels = labels, scores = scores)
}

#' Generate clustering solutions for relabeling analyses
#'
#' Builds the candidate taxonomies that are later relabeled and scored. The
#' function generates the solution families used by Wulff & Mata (2025):
#' hierarchical complete, single, and Ward clustering; k-means; optional
#' Gaussian mixture clustering via `mclust`; optional Louvain community
#' detection; and an optional original taxonomy supplied by the user.
#'
#' @details
#' The input is a scale-scale semantic similarity matrix \eqn{S}. For
#' hierarchical clustering, similarities are converted to distances with
#'
#' \deqn{D_{ij} = 1 - S_{ij}.}
#'
#' Let \eqn{S} be the supplied scale-scale similarity matrix and
#' \eqn{D_{ij} = 1 - S_{ij}} be the dissimilarity used for agglomerative
#' clustering. Three hierarchical linkage rules are evaluated with
#' `stats::hclust(stats::as.dist(D), method = ...)`:
#' \describe{
#'   \item{`hclust_complete`}{complete linkage via `stats::hclust(...,
#'   method = "complete")`, where the distance between candidate clusters is
#'   the largest pairwise dissimilarity,
#'   \eqn{d(A,B) = \max_{i \in A, j \in B} D_{ij}}.}
#'   \item{`hclust_single`}{single linkage via `method = "single"`, where the
#'   distance between candidate clusters is the smallest pairwise dissimilarity,
#'   \eqn{d(A,B) = \min_{i \in A, j \in B} D_{ij}}.}
#'   \item{`hclust_ward`}{Ward's minimum variance method via
#'   `method = "ward.D2"`. This calls R's Ward.D2 implementation directly; no
#'   custom Ward update is implemented inside `embeddcv`.}
#' }
#'
#' For each value in `k_seq`, the hierarchical trees are cut with
#' `stats::cutree(k = k)`. K-means is run directly on the rows of \eqn{S}; that
#' is, each scale is represented by its similarity profile to all scales, and
#' `stats::kmeans()` minimizes the within-cluster squared Euclidean criterion
#' \eqn{\sum_i ||s_i - \mu_{c_i}||^2}. If `mclust` is available and
#' `include_mclust = TRUE`, `mclust::Mclust()` is also run on the rows of
#' \eqn{S} with `G = k`; the returned mixture classification is stored as the
#' solution for that \eqn{k}.
#'
#' Louvain clustering uses a min-max transformed similarity matrix as a weighted
#' undirected graph:
#'
#' \deqn{A_{ij} = \frac{S_{ij} - \min(S)}{\max(S) - \min(S)}.}
#'
#' `igraph::cluster_louvain()` is then evaluated across
#' `louvain_resolution`. Louvain solutions are useful for sensitivity checks;
#' by default the selection helpers exclude them from parsimonious/optimal
#' solution selection so that the main comparison focuses on hierarchical
#' clustering, k-means, and Mclust.
#'
#' @param scale_cos Numeric square scale-scale similarity matrix with identical
#'   row and column names in the same order. Rows/columns are scales; values are
#'   usually cosine similarities, optionally rescaled with
#'   `rescale_similarity()`. The row names become the scale ids used throughout
#'   the returned clustering solutions.
#' @param k_seq Integer vector with target numbers of clusters to evaluate for
#'   hierarchical clustering, k-means, and Mclust. Values should be between 1
#'   and `nrow(scale_cos)`.
#' @param original_solution Optional named vector with an original taxonomy or
#'   cluster assignment. Names must be scale ids matching `rownames(scale_cos)`;
#'   values are cluster ids/construct ids. If supplied, it is stored as
#'   `original_b5` and can be used as a reference solution.
#' @param include_mclust Logical scalar. If `TRUE`, fits `mclust::Mclust()` for
#'   each `k` when the `mclust` package is installed; otherwise returns empty
#'   Mclust slots.
#' @param include_louvain Logical scalar. If `TRUE`, creates Louvain
#'   graph-community solutions with `igraph::cluster_louvain()`.
#' @param louvain_resolution Numeric vector of Louvain resolution values. Each
#'   value is passed to `igraph::cluster_louvain(resolution = ...)`; larger
#'   values usually produce more communities.
#' @param kmeans_nstart Integer scalar. Number of random starts passed to
#'   `stats::kmeans()`. Increase this for more stable k-means solutions.
#' @param kmeans_iter_max Integer scalar. Maximum number of iterations passed to
#'   `stats::kmeans()`.
#' @param seed Integer scalar. Random seed used before k-means and Louvain
#'   clustering.
#'
#' @return A named list. Each top-level element is a clustering family
#'   (`hclust_complete`, `hclust_single`, `hclust_ward`, `kmeans`, `mclust`,
#'   `louvain`, and optionally `original_b5`). Each family contains a list of
#'   named membership vectors, one vector per requested solution. Vector names
#'   are scale ids and vector values are cluster ids.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' set.seed(1)
#' scale_cos <- matrix(runif(25, .2, .9), 5)
#' scale_cos <- (scale_cos + t(scale_cos)) / 2
#' diag(scale_cos) <- 1
#' rownames(scale_cos) <- colnames(scale_cos) <- paste0("scale_", 1:5)
#'
#' sols <- make_relabeling_clustering_solutions(
#'   scale_cos,
#'   k_seq = 2:4,
#'   include_mclust = FALSE,
#'   include_louvain = FALSE
#' )
#' names(sols)
#' sols$hclust_ward[[1]]
#' @export
make_relabeling_clustering_solutions <- function(
    scale_cos,
    k_seq = seq_len(nrow(scale_cos)),
    original_solution = NULL,
    include_mclust = TRUE,
    include_louvain = TRUE,
    louvain_resolution = seq(.5, 5, .05),
    kmeans_nstart = 1,
    kmeans_iter_max = 50,
    seed = 42) {

  scale_cos <- as.matrix(scale_cos)
  hclust_complete <- stats::hclust(stats::as.dist(1 - scale_cos), method = "complete")
  hclust_single <- stats::hclust(stats::as.dist(1 - scale_cos), method = "single")
  hclust_ward <- stats::hclust(stats::as.dist(1 - scale_cos), method = "ward.D2")

  out <- list()
  if (!is.null(original_solution)) {
    out$original_b5 <- list(original_solution)
  }

  out$hclust_complete <- lapply(k_seq, function(k) stats::cutree(hclust_complete, k))
  out$hclust_single <- lapply(k_seq, function(k) stats::cutree(hclust_single, k))
  out$hclust_ward <- lapply(k_seq, function(k) stats::cutree(hclust_ward, k))

  set.seed(seed)
  out$kmeans <- lapply(k_seq, function(k) {
    tryCatch({
      fit <- stats::kmeans(
        scale_cos,
        centers = k,
        nstart = kmeans_nstart,
        iter.max = kmeans_iter_max
      )
      fit$cluster
    }, error = function(e) NULL)
  })

  if (isTRUE(include_mclust) && requireNamespace("mclust", quietly = TRUE)) {
    # mclust::Mclust() evaluates a generated mclustBIC() call in the caller's
    # frame. Keeping this binding local avoids attaching the suggested package.
    mclustBIC <- mclust::mclustBIC
    out$mclust <- lapply(k_seq, function(k) {
      tryCatch({
        fit <- mclust::Mclust(scale_cos, G = k, verbose = FALSE)
        res <- fit$classification
        names(res) <- rownames(scale_cos)
        res
      }, error = function(e) NULL)
    })
  } else {
    out$mclust <- vector("list", length(k_seq))
  }

  if (isTRUE(include_louvain)) {
    set.seed(seed)
    tmp <- (scale_cos - min(scale_cos, na.rm = TRUE)) /
      (max(scale_cos, na.rm = TRUE) - min(scale_cos, na.rm = TRUE))
    graph <- igraph::graph_from_adjacency_matrix(tmp, mode = "undirected", weighted = TRUE)
    out$louvain <- lapply(louvain_resolution, function(resolution) {
      sol <- igraph::cluster_louvain(graph, resolution = resolution)$membership
      names(sol) <- rownames(scale_cos)
      sol
    })
  }

  out
}

#' Relabel and score many clustering solutions
#'
#' Applies the full relabeling score pipeline to every candidate solution
#' generated by `make_relabeling_clustering_solutions()`: first each clustering
#' solution is relabeled with `relabel_solution()`, then jingle and jangle
#' fallacies are counted with `score_label_solution()`.
#'
#' @details
#' The returned table is the main input for the relabeling figure. For each
#' candidate taxonomy it stores the algorithm family, the realized number of
#' clusters \eqn{k}, the number of jingle fallacies, the number of jangle
#' fallacies, their sum, and the vector of assigned labels.
#'
#' For each membership vector \eqn{r}, `score_clustering_solution()` first calls
#' `relabel_solution()` to obtain a scale-to-label assignment
#' \eqn{\hat{\ell}_i}. It then evaluates every unordered scale pair
#' \eqn{i < j}. Let \eqn{S_{ij}} be scale-scale semantic similarity and let
#' \eqn{L_{\hat{\ell}_i\hat{\ell}_j}} be the semantic similarity between the
#' labels assigned to those two scales. Given thresholds
#' \eqn{s_{low}}, \eqn{s_{high}}, \eqn{l_{low}}, and \eqn{l_{high}}, the counts
#' are:
#'
#' \deqn{
#' J_r = \sum_{i < j}
#' I(S_{ij} < s_{low}, L_{\hat{\ell}_i\hat{\ell}_j} > l_{high})
#' }
#'
#' for jingle fallacies and
#'
#' \deqn{
#' A_r = \sum_{i < j}
#' I(S_{ij} > s_{high}, L_{\hat{\ell}_i\hat{\ell}_j} < l_{low})
#' }
#'
#' for jangle fallacies. The solution score is
#'
#' \deqn{F_r = J_r + A_r,}
#'
#' returned as `fallacy_sum`. All other unordered pairs are counted as `good`.
#'
#' Some clustering methods can fail for specific values of `k`, especially
#' k-means and Mclust. `make_relabeling_clustering_solutions()` stores these as
#' `NULL`, and this function silently skips them.
#'
#' @param clustering_solutions Named list from
#'   `make_relabeling_clustering_solutions()`. Each top-level element is a
#'   clustering family and contains a list of named membership vectors.
#' @param scale_label_cross Numeric scale-by-label similarity matrix used for
#'   maximum-weight relabeling. Rows must include all scale ids used in the
#'   clustering membership vectors; columns are candidate label ids.
#' @param scale_cos Numeric square scale-scale similarity matrix used for
#'   fallacy scoring. Row and column names must include the scale ids in the
#'   clustering solutions.
#' @param label_cos Numeric square label-label similarity matrix used for
#'   fallacy scoring. Row and column names must include candidate label ids from
#'   `colnames(scale_label_cross)`.
#' @param thresholds Named numeric vector from `jingle_jangle_thresholds()` with
#'   `scale_low`, `label_low`, `scale_high`, and `label_high`.
#'
#' @return A data frame with one row per valid clustering solution and columns:
#' \describe{
#'   \item{type}{Clustering family, such as `hclust_ward`, `kmeans`, or
#'   `mclust`.}
#'   \item{k}{Number of unique clusters in the solution.}
#'   \item{good, jangle, jingle, fallacy_sum}{Fallacy score counts.}
#'   \item{label_vectors}{List-column containing the relabeled scale-to-label
#'   vector for that solution.}
#' }
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
score_relabeling_solutions <- function(
    clustering_solutions,
    scale_label_cross,
    scale_cos,
    label_cos,
    thresholds) {

  if (is.list(scale_label_cross) && !is.data.frame(scale_label_cross) &&
      !is.null(scale_label_cross$cosim_matrix)) {
    scale_label_cross <- scale_label_cross$cosim_matrix
  }
  n_labels <- ncol(as.matrix(scale_label_cross))

  rows <- list()
  counter <- 1L
  n_skipped <- 0L

  for (type_name in names(clustering_solutions)) {
    sols <- clustering_solutions[[type_name]]
    keep <- !vapply(sols, is.null, logical(1))
    sols <- sols[keep]
    if (length(sols) == 0) next

    for (solution in sols) {
      if (length(unique(solution)) > n_labels) {
        n_skipped <- n_skipped + 1L
        next
      }

      scored <- score_clustering_solution(
        clustering_solution = solution,
        scale_label_cross = scale_label_cross,
        scale_cos = scale_cos,
        label_cos = label_cos,
        thresholds = thresholds
      )
      scores <- scored$scores
      rows[[counter]] <- data.frame(
        type = type_name,
        k = length(unique(solution)),
        good = unname(scores["good"]),
        jangle = unname(scores["jangle"]),
        jingle = unname(scores["jingle"]),
        fallacy_sum = unname(scores["fallacy_sum"]),
        stringsAsFactors = FALSE
      )
      rows[[counter]]$label_vectors <- list(scored$labels)
      counter <- counter + 1L
    }
  }

  if (n_skipped > 0) {
    warning(
      n_skipped, " solution(s) skipped: more clusters than the ", n_labels,
      " candidate labels in scale_label_cross.",
      call. = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Select optimal and parsimonious relabeling solutions
#'
#' Selects two representative solutions for each clustering family: the
#' globally best scoring solution and the best scoring solution under a maximum
#' number of clusters. These are the yellow and grey markers shown in panel B of
#' the relabeling figure.
#'
#' @details
#' Let \eqn{R_a} be all scored solutions for clustering family \eqn{a} after
#' rows in `exclude` have been removed. For any solution \eqn{r \in R_a}, let
#' \eqn{k_r} be its number of clusters and let
#' \eqn{F_r = J_r + A_r} be its fallacy sum, where \eqn{J_r} and \eqn{A_r} are
#' the jingle and jangle counts returned by `score_relabeling_solutions()`.
#'
#' The optimal solution for each family is selected by lexicographic ordering:
#' first minimize fallacy sum and then, only for ties, choose the smaller number
#' of clusters:
#'
#' \deqn{
#' r^{opt}_a =
#' \operatorname*{argmin}_{r \in R_a}(F_r, k_r).
#' }
#'
#' The parsimonious solution applies the same ordering after restricting the
#' candidate set to solutions with fewer than `max_constructs` clusters:
#'
#' \deqn{
#' r^{pars}_a =
#' \operatorname*{argmin}_{r \in R_a: k_r < K}(F_r, k_r),
#' }
#'
#' where \eqn{K} is `max_constructs`. In code this is implemented by filtering
#' to `k < max_constructs`, sorting by `fallacy_sum` and then by `k`, and taking
#' the first row. Thus "parsimonious" means the lowest-fallacy solution under a
#' cluster-count ceiling, not necessarily the lowest-fallacy solution overall.
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   It must contain at least `type`, `k`, `fallacy_sum`, `jangle`, `jingle`,
#'   and the list-column `label_vectors`.
#' @param max_constructs Integer or numeric scalar. Maximum number of clusters
#'   allowed for the
#'   parsimonious solution. The current implementation uses `k < max_constructs`,
#'   matching the Wulff & Mata (2025) relabeling workflow.
#' @param exclude Character vector of values from `labels_scored$type` to
#'   exclude before selecting solutions. By default the original taxonomy and
#'   Louvain sensitivity solutions are excluded.
#'
#' @return Data frame with two rows per algorithm family. The `selection` column
#'   is either `"Optimal"` or `"Parsimonious"`. Panel B displays these fallacy
#'   sums as `Fpars` and `Fopt`; the x-axis positions remain the corresponding
#'   numbers of clusters.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
select_relabeling_solutions <- function(
    labels_scored,
    max_constructs = 100,
    exclude = c("original_b5", "louvain")) {

  dat <- labels_scored[!labels_scored$type %in% exclude, , drop = FALSE]
  types <- unique(dat$type)
  out <- list()

  for (type in types) {
    sub <- dat[dat$type == type, , drop = FALSE]
    sub <- sub[order(sub$fallacy_sum, sub$k), , drop = FALSE]
    optimal <- sub[1, , drop = FALSE]
    optimal$selection <- "Optimal"

    pars <- sub[sub$k < max_constructs, , drop = FALSE]
    pars <- pars[order(pars$fallacy_sum, pars$k), , drop = FALSE]
    pars <- pars[1, , drop = FALSE]
    pars$selection <- "Parsimonious"

    out[[length(out) + 1L]] <- optimal
    out[[length(out) + 1L]] <- pars
  }

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

#' Plot relabeling robustness curves
#'
#' Generates the relabeling robustness figure described by Wulff & Mata (2025).
#' The figure summarizes how the number of jingle, jangle, and total fallacies
#' changes as the number of clusters varies across clustering algorithms.
#' The layout is designed to reproduce the relabeling robustness figure style
#' used in the Supplementary Information of Wulff & Mata (2025).
#'
#' @details
#' The input is the scored solution table returned by
#' `score_relabeling_solutions()`. Original-taxonomy and Louvain rows are
#' removed by default, then scores are averaged by clustering family and number
#' of clusters. The plot facets algorithm families and draws three curves:
#' jingle fallacies, jangle fallacies, and their sum. The optional horizontal
#' reference line is usually the fallacy sum of the original taxonomy.
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   It must contain columns `type`, `k`, `fallacy_sum`, `jangle`, and `jingle`.
#' @param original_fallacy_sum Optional numeric scalar for a horizontal
#'   reference line, typically `score_label_solution(...)[["fallacy_sum"]]` for
#'   the original taxonomy.
#' @param output_file Optional character file path for `ggplot2::ggsave()`. If
#'   `NULL`, the plot is returned but not saved.
#' @param width,height,dpi Numeric scalars. Saved figure settings passed to
#'   `ggplot2::ggsave()` when `output_file` is not `NULL`.
#' @param palette Character scalar. Color palette used for the Jingle, Jangle,
#'   and Total curves.
#'   Options are `"wulff_mata"` (default), `"cividis"`, `"viridis"`,
#'   `"magma"`, `"plasma"`, `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#'   The `"wulff_mata"` palette uses cividis colors close to the palette used
#'   in Wulff & Mata (2025).
#'
#' @return A ggplot object. The y-axis uses a pseudo-log scale so small and
#'   large fallacy counts can be inspected in the same panel.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @importFrom rlang .data
#' @export
plot_relabeling_robustness <- function(
    labels_scored,
    original_fallacy_sum = NULL,
    output_file = NULL,
    width = 10,
    height = 4,
    dpi = 300,
    palette = "wulff_mata") {

  cols <- embeddcv_relabel_palette(3, palette = palette, end = .9)
  dat <- labels_scored[!labels_scored$type %in% c("original_b5", "louvain"), , drop = FALSE]
  dat$type <- dplyr::case_when(
    dat$type == "hclust_complete" ~ "Hclust-complete",
    dat$type == "hclust_single" ~ "Hclust-single",
    dat$type == "hclust_ward" ~ "Hclust-wardD",
    dat$type == "kmeans" ~ "Kmeans",
    dat$type == "mclust" ~ "Mclust",
    TRUE ~ dat$type
  )

  dat <- dat |>
    dplyr::group_by(.data$type, .data$k) |>
    dplyr::summarize(
      Total = mean(.data$fallacy_sum),
      Jingle = mean(.data$jingle),
      Jangle = mean(.data$jangle),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      c("Total", "Jingle", "Jangle"),
      names_to = "Fallacy",
      values_to = "Number of fallacies"
    )
  dat$Fallacy <- factor(dat$Fallacy, c("Jingle", "Jangle", "Total"))

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$k,
      y = .data$`Number of fallacies`,
      group = .data$Fallacy,
      col = .data$Fallacy,
      linewidth = .data$Fallacy
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = cols[c(3, 1, 2)]) +
    ggplot2::scale_linewidth_manual(values = c(1, 1, 1)) +
    ggplot2::facet_grid(. ~ type) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(
      trans = "pseudo_log",
      breaks = c(0, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000)
    )

  if (!is.null(original_fallacy_sum)) {
    p <- p + ggplot2::geom_hline(
      yintercept = original_fallacy_sum,
      linetype = 2,
      linewidth = .2,
      col = "black"
    )
  }

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}

#' Prepare data for a relabeling figure
#'
#' Separates data preparation from plotting for a three-panel relabeling figure
#' in the style of the analyses reported by Wulff & Mata (2025). This is useful
#' when you want to inspect or customize the panel data before drawing, or when
#' you want to redraw the same relabeling solution with different text settings
#' or palettes.
#'
#' @details
#' The function is a convenience wrapper around:
#' \describe{
#'   \item{`prepare_relabeling_panel_a()`}{extracts the fallacy-count curve for
#'   the clustering family used by the selected solution and identifies that
#'   family's parsimonious/optimal solutions.}
#'   \item{`prepare_relabeling_panel_b()`}{selects parsimonious/optimal
#'   solutions for each clustering family.}
#'   \item{`prepare_relabeling_panel_c()`}{selects the final relabeling solution
#'   and computes the circular layout of scales, labels, links, ranks, and
#'   colors.}
#' }
#'
#' The default selection is the global parsimonious relabeling solution: among
#' the candidate clustering families, it selects the row with
#' \eqn{k <} `max_constructs` that minimizes `fallacy_sum`, breaking ties by the
#' smaller `k`. Set `solution = "optimal"` to choose the minimum `fallacy_sum`
#' without the parsimony restriction. Set `solution_type` to a clustering family
#' such as `"hclust_ward"`, `"kmeans"`, or `"mclust"` to select within that
#' family instead of across all families. Panel A then uses the full
#' fallacy-count trajectory for the same clustering family selected for panel C.
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   It must contain the columns used by panels A and B (`type`, `k`,
#'   `fallacy_sum`, `jangle`, `jingle`) and the list-column `label_vectors`
#'   needed by panel C.
#' @param scale_label_cross Numeric scale-by-label similarity matrix used to
#'   compute panel C label ranks and assignments. Rows are scale ids and
#'   columns are candidate label ids. It is usually
#'   `cosim_itens_scales(scale_emb, label_emb)$cosim_matrix`.
#' @param label_by_scale Named character vector mapping each scale id to its
#'   original label id. Names must correspond to row names of
#'   `scale_label_cross`; values must correspond to columns of
#'   `scale_label_cross`.
#' @param max_constructs Integer or numeric scalar. Maximum cluster count for
#'   parsimonious solutions; the comparison is strict (`k < max_constructs`).
#' @param solution Character scalar, either `"parsimonious"` or `"optimal"`.
#'   `"parsimonious"` selects the lowest-fallacy solution with
#'   \eqn{k <} `max_constructs`; `"optimal"` selects the lowest-fallacy solution
#'   without that restriction.
#' @param solution_type Optional character scalar naming a clustering family to
#'   select within, such as `"hclust_ward"`, `"kmeans"`, or `"mclust"`. If
#'   `NULL`, selection is global across all candidate clustering families.
#' @param label_fun Function that accepts scale/label ids and returns display
#'   labels of the same length. The default `scale_label_from_scale()` removes
#'   the instrument prefix, e.g. `"inventory_social_boldness"` becomes
#'   `"social_boldness"`.
#' @param instrument_fun Function that accepts scale ids and returns instrument
#'   labels of the same length. The default `instrument_from_scale()` keeps the
#'   prefix before the first underscore, e.g. `"inventory_social_boldness"`
#'   becomes `"inventory"`.
#' @param palette Character scalar. Palette used when preparing panel C colors.
#'   Options are
#'   `"wulff_mata"` (default), `"cividis"`, `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#' @param verbose Logical scalar. If `TRUE`, prints the selected clustering
#'   family and its parsimonious/optimal fallacy counts.
#'
#' @return A list of class `embeddcv_relabeling_panels` with elements
#'   `panel_a`, `panel_b`, and `panel_c`. Each element can be passed directly to
#'   its corresponding plotting function.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
prepare_relabeling_panels <- function(
    labels_scored,
    scale_label_cross,
    label_by_scale,
    max_constructs = 100,
    solution = c("parsimonious", "optimal"),
    solution_type = NULL,
    label_fun = scale_label_from_scale,
    instrument_fun = instrument_from_scale,
    palette = "wulff_mata",
    verbose = TRUE) {

  solution <- match.arg(solution)

  panel_c <- prepare_relabeling_panel_c(
    labels_scored = labels_scored,
    scale_label_cross = scale_label_cross,
    label_by_scale = label_by_scale,
    max_constructs = max_constructs,
    solution = solution,
    solution_type = solution_type,
    label_fun = label_fun,
    instrument_fun = instrument_fun,
    palette = palette
  )

  out <- list(
    panel_a = prepare_relabeling_panel_a(
      labels_scored,
      max_constructs,
      solution_type = panel_c$solution$type
    ),
    panel_b = prepare_relabeling_panel_b(labels_scored, max_constructs),
    panel_c = panel_c
  )
  class(out) <- c("embeddcv_relabeling_panels", class(out))

  if (isTRUE(verbose)) {
    panel_a_pars <- out$panel_a$parsimonious
    panel_a_opt <- out$panel_a$optimal
    panel_c_solution <- out$panel_c$solution
    panel_c_scope <- if (identical(panel_c$selection_scope, "global")) "global " else ""
    message(
      "Relabeling selected model: Panel A trajectory = ",
      embeddcv_relabel_type_label(panel_a_pars$type),
      " (Fpars = ", panel_a_pars$fallacy_sum,
      "; Fopt = ", panel_a_opt$fallacy_sum,
      "). Panel C shows the ", panel_c_scope, panel_c$solution_kind,
      " ", embeddcv_relabel_type_label(panel_c_solution$type),
      " solution (K = ",
      panel_c_solution$k,
      ", fallacies = ", panel_c_solution$fallacy_sum, ")."
    )
  }

  out
}

#' Prepare panel A relabeling data
#'
#' Extracts the clustering solutions needed for panel A of a relabeling figure
#' in the style of the analyses reported by Wulff & Mata (2025). Panel A plots
#' fallacy counts as a function of the number of clusters for one clustering
#' family.
#'
#' @details
#' If `solution_type = NULL`, the panel uses the clustering family of the global
#' parsimonious solution: among non-original and non-Louvain rows with
#' \eqn{k <} `max_constructs`, it selects the row with the smallest
#' `fallacy_sum`, breaking ties by smaller `k`, and then plots the full
#' trajectory for that row's `type`. You can set `solution_type` explicitly to
#' draw a particular family, such as `"hclust_ward"`, `"kmeans"`, or `"mclust"`.
#'
#' It stores the total, jingle, and jangle fallacy curves, the original taxonomy
#' fallacy reference line (if present), and two selected solutions from the same
#' clustering family:
#'
#' \deqn{\textrm{parsimonious} =
#' \operatorname*{argmin}_{k < \textrm{max\_constructs}}
#' (\textrm{fallacy\_sum}, k)}
#'
#' and
#'
#' \deqn{\textrm{optimal} =
#' \operatorname*{argmin}(\textrm{fallacy\_sum}, k).}
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   It must contain at least one candidate clustering family and columns
#'   `type`, `k`, `fallacy_sum`, `jangle`, and `jingle`. A row with
#'   `type == "original_b5"` is optional and, if present, supplies the reference
#'   fallacy line.
#' @param max_constructs Integer or numeric scalar. Maximum cluster count for
#'   parsimonious solutions. The comparison is strict (`k < max_constructs`).
#' @param solution_type Optional character scalar naming the clustering family
#'   to plot, such as `"hclust_ward"`, `"kmeans"`, or `"mclust"`. If `NULL`,
#'   the family of the global parsimonious solution is used.
#'
#' @return A list consumed by `plot_relabeling_panel_a()`, including the
#'   solution table, selected solutions, axis ticks, and plotting ranges.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
prepare_relabeling_panel_a <- function(labels_scored, max_constructs = 100, solution_type = NULL) {
  if (is.null(solution_type)) {
    candidates <- labels_scored[
      !labels_scored$type %in% c("original_b5", "louvain") &
        labels_scored$k < max_constructs,
      ,
      drop = FALSE
    ]
    candidates <- candidates[order(candidates$fallacy_sum, candidates$k), , drop = FALSE]
    if (nrow(candidates) == 0) {
      stop("No relabeling solution is below `max_constructs`.", call. = FALSE)
    }
    solution_type <- candidates$type[1]
  }

  solutions_curve <- labels_scored[labels_scored$type == solution_type, , drop = FALSE]
  solutions_curve <- solutions_curve[order(solutions_curve$k), , drop = FALSE]
  if (nrow(solutions_curve) == 0) {
    stop("`labels_scored` does not contain solutions for `solution_type = ",
         solution_type, "`.", call. = FALSE)
  }

  original <- labels_scored[labels_scored$type == "original_b5", , drop = FALSE]
  original_sum <- if (nrow(original)) original$fallacy_sum[1] else NA_real_
  panel_a_xmax <- ceiling(max(solutions_curve$k) / 30) * 30 + 10.5

  pars <- solutions_curve[solutions_curve$k < max_constructs, , drop = FALSE]
  if (nrow(pars) == 0) {
    stop("No ", solution_type, " solution is below `max_constructs`.", call. = FALSE)
  }
  pars <- pars[order(pars$fallacy_sum, pars$k), , drop = FALSE][1, , drop = FALSE]
  opt <- solutions_curve[order(solutions_curve$fallacy_sum, solutions_curve$k), , drop = FALSE][1, , drop = FALSE]

  xlocs <- c(pars$k, opt$k)
  ylocs <- c(pars$fallacy_sum, opt$fallacy_sum)

  list(
    solution_type = solution_type,
    solutions_curve = solutions_curve,
    solutions_ward = solutions_curve,
    original_sum = original_sum,
    parsimonious = pars,
    optimal = opt,
    xlocs = xlocs,
    ylocs = ylocs,
    panel_a_xmax = panel_a_xmax,
    xticks = unique(c(1, seq(30, ceiling(max(solutions_curve$k) / 30) * 30, 30))),
    yticks = c(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000)
  )
}

#' Prepare panel B relabeling data
#'
#' Selects and formats parsimonious and optimal solutions for each clustering
#' family. Panel B displays these two selected cluster counts on a horizontal
#' scale, with point size reflecting the number of fallacies. This prepares the
#' algorithm-comparison panel in the style of the relabeling analyses reported
#' by Wulff & Mata (2025).
#'
#' @details
#' The same selection rule as `select_relabeling_solutions()` is used within
#' each algorithm: minimize `fallacy_sum`, then minimize `k` to break ties. If
#' the parsimonious and optimal solutions have the same `k`, their x positions
#' are nudged slightly apart to avoid overplotting in the panel.
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   It must contain `type`, `k`, and `fallacy_sum` columns for one or more
#'   clustering families.
#' @param max_constructs Integer or numeric scalar. Maximum cluster count for
#'   parsimonious solutions. The comparison is strict (`k < max_constructs`).
#'
#' @return A list consumed by `plot_relabeling_panel_b()`, including selected
#'   rows, nudged x positions, type labels, x-axis ticks, and x-axis limits.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#' @export
prepare_relabeling_panel_b <- function(labels_scored, max_constructs = 100) {
  dat <- labels_scored[!labels_scored$type %in% c("original_b5", "louvain"), , drop = FALSE]
  type_order <- c("hclust_single", "kmeans", "mclust", "hclust_complete", "hclust_ward")
  type_labels <- c(
    hclust_single = "HC single",
    kmeans = "Kmeans",
    mclust = "Mclust",
    hclust_complete = "HC complete",
    hclust_ward = "HC ward"
  )
  dat <- dat[dat$type %in% type_order, , drop = FALSE]
  types <- type_order[type_order %in% unique(dat$type)]

  opt <- do.call(rbind, lapply(types, function(type) {
    sub <- dat[dat$type == type, , drop = FALSE]
    sub[order(sub$fallacy_sum, sub$k), , drop = FALSE][1, , drop = FALSE]
  }))
  pars <- do.call(rbind, lapply(types, function(type) {
    sub <- dat[dat$type == type & dat$k < max_constructs, , drop = FALSE]
    sub[order(sub$fallacy_sum, sub$k), , drop = FALSE][1, , drop = FALSE]
  }))

  pars_x <- pars$k
  opt_x <- opt$k
  overlap <- pars_x == opt_x
  pars_x[overlap] <- pmax(pars_x[overlap] - 2, 0)
  opt_x[overlap] <- opt_x[overlap] + 2

  list(
    opt = opt,
    parsimonious = pars,
    pars_x = pars_x,
    opt_x = opt_x,
    type_labels = type_labels,
    xticks = seq(0, max(labels_scored$k, na.rm = TRUE), 30),
    xlim = c(0, max(labels_scored$k, na.rm = TRUE) + 45)
  )
}

#' Prepare panel C circular relabeling data
#'
#' Builds the circular assignment layout used in panel C of the relabeling
#' figure. The function chooses a relabeling solution, extracts its assigned
#' labels, ranks those labels for each scale, and computes the inner/outer
#' circle coordinates used by `plot_relabeling_panel_c()`. This prepares the
#' circular relabeling panel in the style of the relabeling analyses reported by
#' Wulff & Mata (2025).
#'
#' @details
#' Let \eqn{F_r = J_r + A_r} be the total fallacy count for candidate solution
#' \eqn{r}, where \eqn{J_r} is the number of jingle fallacies and \eqn{A_r} is
#' the number of jangle fallacies returned by `score_label_solution()`.
#' With `solution = "parsimonious"`, the selected row is
#'
#' \deqn{
#' r^* = \operatorname*{argmin}_{r: k_r < K} (F_r, k_r),
#' }
#'
#' where \eqn{K} is `max_constructs` and ties are broken by smaller
#' \eqn{k_r}. With `solution = "optimal"`, the same rule is applied without the
#' \eqn{k_r < K} restriction:
#'
#' \deqn{
#' r^* = \operatorname*{argmin}_{r} (F_r, k_r).
#' }
#'
#' If `solution_type` is supplied, this optimization is restricted to that
#' clustering family; otherwise it is global across all non-original,
#' non-Louvain candidate families.
#'
#' For each scale \eqn{i} and assigned label \eqn{\ell}, the label rank is
#' computed from the row of `scale_label_cross`:
#'
#' \deqn{\textrm{rank}_{i\ell} = 1 + \#\{m: M_{im} > M_{i\ell}\}.}
#'
#' A link is marked as `covered` when the assigned label is among the five most
#' similar labels for that scale (`rank <= 5`). In the plot, covered links are
#' drawn as solid lines and uncovered links as dashed/dotted lines, paralleling
#' the distinction in Wulff & Mata (2025) between assignments with optimal
#' top-five similarity and assignments with suboptimal similarity.
#'
#' Scales are placed around the outer circle in sorted assignment order. Labels
#' are placed on the inner circle at the circular mean of the angles of their
#' assigned scales. Larger label points indicate that more scales were assigned
#' to that label. The circular-coordinate and color-blending helpers use local
#' adaptations of plotting utilities from the `memnet` package (Wulff, 2019):
#' `embeddcv_circle_raw()` follows the geometry of the internal `circle_raw()`
#' helper, and `mix_col()` follows the RGB interpolation behavior of
#' `memnet::cmix()`.
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   It must contain `type`, `k`, `fallacy_sum`, and a list-column
#'   `label_vectors`, where each list element is a named scale-to-label vector.
#' @param scale_label_cross Numeric scale-by-label similarity matrix. Rows are
#'   scale ids; columns are candidate label ids. It must include the scale ids
#'   and assigned label ids from the selected `label_vectors` entry.
#' @param label_by_scale Named character vector mapping each scale id to its
#'   original label id. Names must be scale ids and values must be column names
#'   in `scale_label_cross`.
#' @param max_constructs Integer or numeric scalar. Maximum cluster count for
#'   parsimonious solutions. The comparison is strict (`k < max_constructs`).
#' @param solution Character scalar, either `"parsimonious"` or `"optimal"`.
#'   `"parsimonious"` selects the lowest-fallacy solution with
#'   \eqn{k <} `max_constructs`; `"optimal"` selects the lowest-fallacy solution
#'   without that restriction.
#' @param solution_type Optional character scalar naming a clustering family to
#'   select within, such as `"hclust_ward"`, `"kmeans"`, or `"mclust"`. If
#'   `NULL`, selection is global across candidate families.
#' @param label_fun Function that accepts scale/label ids and returns display
#'   labels of the same length.
#' @param instrument_fun Function that accepts scale ids and returns display
#'   instrument labels of the same length.
#' @param palette Character scalar. Palette used for label groups. Options are
#'   `"wulff_mata"` (default), `"cividis"`, `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#'
#' @return A list consumed by `plot_relabeling_panel_c()`, including the final
#'   solution row, scale-to-label assignments, label ranks, circle coordinates,
#'   and color mapping.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' Wulff, D. U. (2019). memnet: Network Tools for Memory Research. R package.
#' https://www.rdocumentation.org/packages/memnet/versions/0.1.0
#' @export
prepare_relabeling_panel_c <- function(
    labels_scored,
    scale_label_cross,
    label_by_scale,
    max_constructs = 100,
    solution = c("parsimonious", "optimal"),
    solution_type = NULL,
    label_fun = scale_label_from_scale,
    instrument_fun = instrument_from_scale,
    palette = "wulff_mata") {

  solution <- match.arg(solution)

  dat <- labels_scored[!labels_scored$type %in% c("original_b5", "louvain"), , drop = FALSE]
  selection_scope <- "global"
  if (!is.null(solution_type)) {
    dat <- dat[dat$type == solution_type, , drop = FALSE]
    selection_scope <- "selected"
  }
  if (identical(solution, "parsimonious")) {
    dat <- dat[dat$k < max_constructs, , drop = FALSE]
  }
  dat <- dat[order(dat$fallacy_sum, dat$k), , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("No ", solution, " relabeling solution is available for the requested selection.", call. = FALSE)
  }
  labels_final <- dat$label_vectors[[1]]

  ranks <- t(apply(scale_label_cross, 1, function(x) rank(-x, ties.method = "first")))
  assignment_tbl <- data.frame(
    label_name = as.character(labels_final),
    scale = names(labels_final),
    stringsAsFactors = FALSE
  )
  assignment_tbl$scale_name <- paste0(label_fun(assignment_tbl$scale), " (", instrument_fun(assignment_tbl$scale), ")")
  count_tbl <- table(labels_final)
  assignment_tbl$n <- as.integer(count_tbl[assignment_tbl$label_name])
  assignment_tbl$label_original <- label_by_scale[assignment_tbl$scale]
  assignment_tbl$rank <- ranks[cbind(assignment_tbl$scale, assignment_tbl$label_name)]
  assignment_tbl$rank_original <- ranks[cbind(assignment_tbl$scale, assignment_tbl$label_original)]
  assignment_tbl$covered <- assignment_tbl$rank <= 5
  assignment_tbl <- assignment_tbl[order(-assignment_tbl$n, assignment_tbl$label_name,
                                         !assignment_tbl$covered, assignment_tbl$scale_name), ]

  uni_labels <- unique(assignment_tbl$label_name)
  index <- seq_along(uni_labels)
  names(index) <- uni_labels
  cols <- embeddcv_relabel_palette(length(uni_labels), palette = palette, begin = .1, end = .9)
  names(cols) <- uni_labels
  assignment_tbl$col <- cols[assignment_tbl$label_name]
  assignment_tbl$index <- index[assignment_tbl$label_name]

  n_scales <- nrow(assignment_tbl)
  deg <- seq(0, 360 - (360 / n_scales), length = n_scales)
  deg <- deg - (360 / n_scales) * 13.5
  deg <- ifelse(deg < 0, 360 + deg, deg)
  assignment_tbl$deg <- deg

  label_deg <- stats::aggregate(
    deg ~ label_name,
    data = assignment_tbl,
    FUN = embeddcv_circular_mean_deg
  )
  label_deg$n <- as.integer(table(assignment_tbl$label_name)[label_deg$label_name])
  label_deg$size <- .5 + label_deg$n / 9
  label_deg <- label_deg[match(uni_labels, label_deg$label_name), ]

  outer <- t(sapply(assignment_tbl$deg, function(x) embeddcv_circle_point(x, 1)))
  outer_text <- t(sapply(assignment_tbl$deg, function(x) embeddcv_circle_point(x, 1.02)))
  rownames(outer) <- rownames(outer_text) <- assignment_tbl$scale_name
  inner <- t(sapply(label_deg$deg, function(x) embeddcv_circle_point(x, .5)))
  inner_text <- t(sapply(label_deg$deg, function(x) embeddcv_circle_point(x, .55)))
  rownames(inner) <- rownames(inner_text) <- label_deg$label_name

  list(
    solution = dat[1, , drop = FALSE],
    solution_kind = solution,
    selection_scope = selection_scope,
    labels_final = labels_final,
    assignment_tbl = assignment_tbl,
    label_deg = label_deg,
    outer = outer,
    outer_text = outer_text,
    inner = inner,
    inner_text = inner_text,
    cols = cols,
    label_fun = label_fun
  )
}

#' Plot panel A of the relabeling figure
#'
#' Draws one relabeling curve: number of clusters on the x-axis and fallacy
#' counts on the log-scaled y-axis. The thick middle curve is the total fallacy
#' sum; the two thinner curves show jangle and jingle fallacies
#' separately.
#'
#' @param panel_a List returned by `prepare_relabeling_panel_a()`. It must
#'   contain `solutions_curve`, `original_sum`, selected `parsimonious` and
#'   `optimal` rows, `xlocs`, `ylocs`, axis ticks, and plotting limits.
#' @param fallacy_sum_text,jingle_fallacies_text,jangle_fallacies_text Optional
#'   named lists used to override direct curve-label annotations when
#'   `curve_legend = FALSE`. Supported fields are `x`, `y`, `label`, `adj`,
#'   `cex`, `font`, and `col`, matching arguments passed to `graphics::text()`.
#'   For example, `fallacy_sum_text = list(x = 145, y = log(500), cex = .9)`
#'   moves the total fallacy label and changes its size.
#' @param parsimonious_solution_text,optimal_solution_text Optional named lists
#'   used to override the solution annotations. Supported fields are `x`, `y`,
#'   `label`, `adj`, `cex`, `font`, and `col`. These lists also accept
#'   `x_offset`, which is interpreted relative to the selected solution's
#'   number of clusters. For example,
#'   `parsimonious_solution_text = list(x_offset = 8, y = log(8000))` places the
#'   annotation eight clusters to the right of the parsimonious marker.
#' @param panel_letter Character scalar used as the panel label, usually `"A"`.
#'   Use `NULL` to suppress it.
#' @param palette Character scalar. Curve color palette. Options are
#'   `"wulff_mata"` (default),
#'   `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`,
#'   `"okabe_ito"`, and `"blue_gold"`. The same palette controls the total,
#'   jingle, and jangle curves.
#' @param curve_legend Logical scalar. If `TRUE` (default), the fallacy-sum,
#'   jingle, and jangle curves are identified by a compact line legend instead
#'   of direct labels near the curves.
#' @param curve_legend_position Character scalar passed to `graphics::legend()`.
#'   Useful values are `"topright"`, `"topleft"`, `"bottomright"`, and
#'   `"bottomleft"`.
#' @param curve_legend_inset Numeric scalar or length-two numeric vector passed
#'   to `graphics::legend(inset = ...)`, used to move the legend inward from
#'   the selected corner.
#' @param curve_legend_cex Numeric scalar. Text size for the panel A curve
#'   legend.
#'
#' @return Invisibly returns `panel_a`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' \dontrun{
#' panel_a <- prepare_relabeling_panel_a(labels_scored)
#' plot_relabeling_panel_a(
#'   panel_a,
#'   curve_legend = FALSE,
#'   fallacy_sum_text = list(x = 145, y = log(500)),
#'   jingle_fallacies_text = list(x = 210, y = log(350)),
#'   palette = "blue_gold"
#' )
#' }
#' @export
plot_relabeling_panel_a <- function(
    panel_a,
    fallacy_sum_text = NULL,
    jingle_fallacies_text = NULL,
    jangle_fallacies_text = NULL,
    parsimonious_solution_text = NULL,
    optimal_solution_text = NULL,
    panel_letter = "A",
    palette = "wulff_mata",
    curve_legend = TRUE,
    curve_legend_position = "topright",
    curve_legend_inset = .02,
    curve_legend_cex = .82) {

  solutions_curve <- if (!is.null(panel_a$solutions_curve)) panel_a$solutions_curve else panel_a$solutions_ward
  original_sum <- panel_a$original_sum
  cols <- embeddcv_relabel_palette(10, palette = palette, begin = 0, end = .85)

  graphics::par(mar = c(4, 5, 2.5, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(.5, panel_a$panel_a_xmax), ylim = c(0, 12))

  if (is.finite(original_sum)) {
    graphics::lines(c(1, panel_a$panel_a_xmax + 30), log(c(original_sum, original_sum)), lty = 2)
  }
  graphics::lines(solutions_curve$k, log(pmax(solutions_curve$fallacy_sum, 1)), lwd = 12, col = cols[6])
  graphics::lines(solutions_curve$k, log(pmax(solutions_curve$jangle, 1)), lwd = 5, col = cols[1])
  graphics::lines(solutions_curve$k, log(pmax(solutions_curve$jingle, 1)), lwd = 5, col = cols[10])

  graphics::mtext(panel_a$xticks, at = panel_a$xticks, side = 1, cex = .8)
  graphics::mtext(panel_a$yticks, at = log(panel_a$yticks), side = 2, cex = .8, las = 1)
  graphics::mtext(c("Number of clusters", "Number of fallacies"), side = c(1, 2), line = c(2, 3), cex = 1.2)

  xlocs <- panel_a$xlocs
  ylocs <- panel_a$ylocs
  graphics::points(xlocs, log(pmax(ylocs, 1)), pch = 16, cex = 1)
  graphics::lines(c(xlocs[1], xlocs[1]), log(c(max(ylocs[1], 1), max(10000, original_sum, na.rm = TRUE))), lwd = 1)
  graphics::lines(c(xlocs[2], xlocs[2]), log(c(max(ylocs[2], 1), max(10000, original_sum, na.rm = TRUE))), lwd = 1)

  pars_text <- embeddcv_relabel_text_spec(
    list(x = xlocs[1] + 5, y = log(5000), label = "Parsimonious\nsolution", font = 3, adj = 0),
    parsimonious_solution_text,
    anchor_x = xlocs[1]
  )
  opt_text <- embeddcv_relabel_text_spec(
    list(x = xlocs[2] + 5, y = log(5000), label = "Optimal\nsolution", font = 3, adj = 0),
    optimal_solution_text,
    anchor_x = xlocs[2]
  )
  embeddcv_draw_relabel_text(pars_text)
  embeddcv_draw_relabel_text(opt_text)

  if (isTRUE(curve_legend)) {
    graphics::legend(
      curve_legend_position,
      legend = c("Fallacy sum", "Jingle fallacies", "Jangle fallacies"),
      col = c(cols[6], cols[10], cols[1]),
      lwd = c(7, 4, 4),
      lty = 1,
      bty = "n",
      cex = curve_legend_cex,
      inset = curve_legend_inset,
      x.intersp = .55,
      y.intersp = .9,
      seg.len = 1.25
    )
  } else {
    jangle_text <- embeddcv_relabel_text_spec(
      list(x = 40, y = log(3), label = "Jangle\nfallacies", adj = 0, cex = 1, col = cols[1]),
      jangle_fallacies_text
    )
    jingle_text <- embeddcv_relabel_text_spec(
      list(x = 222, y = log(285), label = "Jingle\nfallacies", adj = 0, cex = 1, col = cols[10]),
      jingle_fallacies_text
    )
    fallacy_text <- embeddcv_relabel_text_spec(
      list(x = 142, y = log(360), label = "Fallacy\nsum", adj = 0, cex = 1, col = cols[6]),
      fallacy_sum_text
    )

    embeddcv_draw_relabel_text(jangle_text)
    embeddcv_draw_relabel_text(jingle_text)
    embeddcv_draw_relabel_text(fallacy_text)
  }

  if (!is.null(panel_letter)) {
    graphics::mtext(panel_letter, side = 3, at = -45, font = 2, cex = 2, line = .3)
  }

  invisible(panel_a)
}

#' Plot panel B of the relabeling figure
#'
#' Draws the algorithm comparison panel. For each clustering method, the
#' parsimonious and optimal selected solutions are shown on the x-axis as number
#' of clusters. Point diameter scales with fallacy count. The panel follows the
#' relabeling comparison display described by Wulff & Mata (2025).
#' The text under each algorithm is formatted as
#' `(Fpars = ...; Fopt = ...)`, where `Fpars` is the parsimonious solution's
#' fallacy sum and `Fopt` is the optimal solution's fallacy sum.
#'
#' @param panel_b List returned by `prepare_relabeling_panel_b()`. It must
#'   contain selected optimal and parsimonious rows (`opt`, `parsimonious`),
#'   their plotted x positions (`opt_x`, `pars_x`), display labels, ticks, and
#'   x-axis limits.
#' @param panel_letter Character scalar used as the panel label, usually `"B"`.
#'   Use `NULL` to suppress it.
#' @param palette Character scalar. Point color palette. Options are
#'   `"wulff_mata"` (default),
#'   `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`,
#'   `"okabe_ito"`, and `"blue_gold"`. The palette controls parsimonious and
#'   optimal solution markers.
#'
#' @return Invisibly returns `panel_b`.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. Nature Human
#' Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @examples
#' \dontrun{
#' panel_b <- prepare_relabeling_panel_b(labels_scored)
#' plot_relabeling_panel_b(panel_b, palette = "okabe_ito")
#' }
#' @export
plot_relabeling_panel_b <- function(panel_b, panel_letter = "B", palette = "wulff_mata") {
  opt <- panel_b$opt
  pars <- panel_b$parsimonious
  cols <- embeddcv_relabel_palette(3, palette = palette, end = .9)
  pars_diameter <- (pmax(pars$fallacy_sum, 1) / pi)^.29
  opt_diameter <- (pmax(opt$fallacy_sum, 1) / pi)^.29

  graphics::par(mar = c(0, 10, 0, 0))
  graphics::plot.new()
  graphics::plot.window(ylim = c(.2, nrow(opt) + 1.2), xlim = panel_b$xlim)

  for (i in seq_len(nrow(opt))) {
    graphics::lines(c(panel_b$pars_x[i], panel_b$opt_x[i]), c(i, i),
                    col = "black", lwd = 1.5, lend = 2, lty = 3)
  }
  graphics::points(panel_b$pars_x, seq_len(nrow(pars)), cex = pars_diameter, pch = 21, bg = cols[3], col = "white")
  graphics::points(panel_b$opt_x, seq_len(nrow(opt)), cex = opt_diameter, pch = 21, bg = cols[2], col = "white")
  last_y <- nrow(opt)
  graphics::text(pars$k[last_y] + 5, y = last_y + .8, labels = "Parsimonious\nsolution", font = 3, adj = 1)
  graphics::text(opt$k[last_y] - 5, y = last_y + .8, labels = "Optimal\nsolution", font = 3, adj = 0)
  graphics::mtext(panel_b$type_labels[opt$type], at = seq_len(nrow(opt)) + .15, side = 2, las = 2, adj = 1, cex = 1.2)
  graphics::mtext(
    paste0("(Fpars = ", pars$fallacy_sum, "; Fopt = ", opt$fallacy_sum, ")"),
    at = seq_len(nrow(opt)) - .2,
    side = 2,
    las = 2,
    adj = 1,
    cex = .8
  )
  graphics::mtext(panel_b$xticks, at = panel_b$xticks, side = 1, cex = .8)
  graphics::mtext("Number of clusters", side = 1, line = 1.5, cex = 1.2)

  if (!is.null(panel_letter)) {
    graphics::mtext(panel_letter, side = 3, at = -90, font = 2, cex = 2, line = -2.5)
  }

  invisible(panel_b)
}

#' Plot panel C of the relabeling figure
#'
#' Draws the circular relabeling map. Outer points are original scales; inner
#' points are relabeled constructs. Lines connect each scale to its assigned
#' construct label. Solid lines indicate that the assigned label is among the
#' top five labels for that scale according to `scale_label_cross`; dashed or
#' dotted lines indicate lower-ranked assignments. In other words, solid and
#' dashed/dotted lines distinguish top-five semantic matches from suboptimal
#' similarity assignments. The panel follows the circular relabeling display
#' described by Wulff & Mata (2025).
#' The circular coordinates and color blending used by this panel are local
#' implementations adapted from plotting utilities in the `memnet` package
#' (Wulff, 2019): the coordinates follow the logic of its internal
#' `circle_raw()` helper, and color blending follows `memnet::cmix()`.
#'
#' @param panel_c List returned by `prepare_relabeling_panel_c()`. It must
#'   contain `assignment_tbl`, `label_deg`, outer/inner circle coordinates,
#'   color mapping, and the display `label_fun`.
#' @param panel_letter Character scalar used as the panel label, usually `"C"`.
#'   Use `NULL` to suppress it.
#' @param palette Optional character scalar palette override for label groups.
#'   If `NULL`, the
#'   colors stored in `panel_c` are used. Otherwise options are
#'   `"wulff_mata"`, `"cividis"`, `"viridis"`, `"magma"`, `"plasma"`,
#'   `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#'
#' @return Invisibly returns `panel_c`.
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
#' @examples
#' \dontrun{
#' panel_c <- prepare_relabeling_panel_c(
#'   labels_scored,
#'   scale_label_cross = scale_label_cross,
#'   label_by_scale = label_by_scale
#' )
#' plot_relabeling_panel_c(panel_c, palette = "plasma")
#' }
#' @export
plot_relabeling_panel_c <- function(panel_c, panel_letter = "C", palette = NULL) {
  assignment_tbl <- panel_c$assignment_tbl
  label_deg <- panel_c$label_deg
  outer <- panel_c$outer
  outer_text <- panel_c$outer_text
  inner <- panel_c$inner
  inner_text <- panel_c$inner_text
  cols <- panel_c$cols
  label_fun <- panel_c$label_fun

  if (!is.null(palette)) {
    label_names <- names(cols)
    cols <- embeddcv_relabel_palette(length(label_names), palette = palette, begin = .1, end = .9)
    names(cols) <- label_names
    assignment_tbl$col <- cols[assignment_tbl$label_name]
  }

  graphics::par(mar = c(0, 1, 0, 1))
  lim <- .4
  yshift <- .1
  xshift <- -.05
  graphics::plot.new()
  graphics::plot.window(xlim = c(-1 - lim + xshift, 1 + lim + xshift),
                        ylim = c(-1 - lim + yshift, 1 + lim + yshift))

  for (i in seq_len(nrow(assignment_tbl))) {
    pair <- c(assignment_tbl$label_name[i], assignment_tbl$scale_name[i])
    p1 <- outer[pair[2], ]
    p2 <- inner[pair[1], ]
    graphics::lines(c(p1[1], p2[1]), c(p1[2], p2[2]), lwd = 1,
                    col = assignment_tbl$col[i],
                    lty = ifelse(assignment_tbl$covered[i], 1, 3))
  }

  for (i in seq_len(nrow(assignment_tbl))) {
    pair <- c(assignment_tbl$label_name[i], assignment_tbl$scale_name[i])
    n <- assignment_tbl$n[i]
    elements <- strsplit(pair[1], "\n", fixed = TRUE)[[1]]
    width <- max(graphics::strwidth(elements))
    p1 <- outer[pair[2], ]
    p2 <- inner[pair[1], ]
    p_new <- (p1 - p2) * (width^.55 * n^.3) / 1.35 + p2
    graphics::lines(c(p_new[1], p2[1]), c(p_new[2], p2[2]), lwd = 1,
                    col = mix_col(assignment_tbl$col[i], "white", .85), lty = 1)
  }

  graphics::points(outer, pch = 21, cex = .6,
                   bg = mix_col(assignment_tbl$col, "white", .2),
                   col = mix_col(assignment_tbl$col, "black", .2))
  label_cols <- cols[rownames(inner)]
  graphics::points(inner, pch = 21, bg = mix_col(label_cols, "white", .2),
                   col = mix_col(label_cols, "black", .2), cex = label_deg$size)

  for (i in seq_len(nrow(outer))) {
    rot <- 90
    ang <- embeddcv_rev_angle(embeddcv_rotate(assignment_tbl$deg[i], rot))
    graphics::text(outer_text[i, 1], outer_text[i, 2], labels = rownames(outer)[i],
                   srt = ang,
                   adj = ifelse(embeddcv_test_angle(embeddcv_rotate(assignment_tbl$deg[i], rot)), 0, 1),
                   cex = .4)
  }

  graphics::par(lheight = .7)
  for (i in seq_len(nrow(inner))) {
    n_i <- label_deg$n[i]
    graphics::text(inner_text[i, 1], inner_text[i, 2],
                   labels = capitalize_first(label_fun(rownames(inner)[i])),
                   srt = embeddcv_rev_angle(embeddcv_rotate(label_deg$deg[i], 90)),
                   adj = ifelse(embeddcv_test_angle(embeddcv_rotate(label_deg$deg[i], 90)), 0, 1),
                   cex = n_i^.4 / 2 - .3, font = 1)
  }

  if (!is.null(panel_letter)) {
    graphics::mtext(panel_letter, side = 3, at = -1.555, font = 2, cex = 2, line = -8)
  }

  invisible(panel_c)
}

#' Plot the three-panel Wulff & Mata-style relabeling figure
#'
#' Generates the relabeling figure in the style of Wulff & Mata (2025). The
#' figure summarizes how semantic relabeling can reduce taxonomic
#' incommensurability. By default, panel A and panel C are keyed to the same
#' global parsimonious solution. Panel A shows how jingle, jangle, and total
#' fallacy counts change across that solution's clustering family. Panel B
#' compares parsimonious and optimal solutions across clustering algorithms.
#' Panel C draws the circular relabeling assignment map for the selected
#' solution; solid links denote assignments among a scale's top five candidate
#' labels and dashed/dotted links denote lower-ranked assignments.
#' This function is a high-level wrapper around
#' `prepare_relabeling_panels()`, `plot_relabeling_panel_a()`,
#' `plot_relabeling_panel_b()`, and `plot_relabeling_panel_c()`.
#'
#' @details
#' If `relabeling_panels` is not supplied, the function prepares all panel data
#' internally from `labels_scored`, `scale_label_cross`, and `label_by_scale`.
#' Supplying a precomputed `relabeling_panels` object is useful when the data are
#' expensive to prepare or when you want to inspect the selected solution before
#' plotting.
#'
#' With the default `solution = "parsimonious"` and `solution_type = NULL`, the
#' selected solution minimizes `fallacy_sum` among candidate clustering
#' solutions with \eqn{k <} `max_constructs`, breaking ties by smaller `k`.
#' With `solution = "optimal"`, the selected solution minimizes `fallacy_sum`
#' without the \eqn{k <} `max_constructs` restriction. If `solution_type` is
#' supplied, selection is restricted to that clustering family instead of being
#' global. Panel A plots the full trajectory for the same clustering family as
#' this selected solution; panel C draws the selected solution as the circular
#' scale-to-label map.
#'
#' The circular label layout and color-mixing helpers used for panel C are local
#' implementations adapted from plotting utilities in the `memnet` package
#' (Wulff, 2019): `embeddcv_circle_raw()` follows the geometry of its internal
#' `circle_raw()` helper, and `mix_col()` follows the RGB interpolation behavior
#' of `memnet::cmix()`.
#'
#' The PNG device is opened with `grDevices::png(width = width, height = height,
#' res = res, units = "in")`. Thus `width` and `height` are in inches, and
#' `res` is pixels per inch. The final pixel size is approximately
#' `width * res` by `height * res`.
#'
#' @param labels_scored Data frame returned by `score_relabeling_solutions()`.
#'   Required when `relabeling_panels = NULL`; ignored when `relabeling_panels`
#'   is supplied.
#' @param scale_label_cross Numeric scale-by-label similarity matrix. Required
#'   when `relabeling_panels = NULL`. Rows are scale ids; columns are candidate
#'   label ids.
#' @param label_by_scale Named character vector mapping each scale id to its
#'   original label id. Required when `relabeling_panels = NULL`. Names must be
#'   scale ids and values must be column names in `scale_label_cross`.
#' @param relabeling_panels Optional list returned by
#'   `prepare_relabeling_panels()`. If supplied, the three panels are drawn from
#'   this object and the raw preparation inputs are ignored.
#' @param output_file Optional character PNG path. If `NULL`, the plot is drawn
#'   on the active graphics device and no file is written.
#' @param max_constructs Integer or numeric scalar. Maximum cluster count for
#'   parsimonious solutions when panel data must be prepared internally.
#' @param solution Character scalar, either `"parsimonious"` or `"optimal"`.
#'   Used only when `relabeling_panels = NULL`.
#' @param solution_type Optional character scalar naming a clustering family to
#'   select within, such as `"hclust_ward"`, `"kmeans"`, or `"mclust"`. If
#'   `NULL`, selection is global across candidate families. Used only when
#'   `relabeling_panels = NULL`.
#' @param width,height Numeric scalars. PNG size in inches when `output_file` is
#'   supplied.
#' @param res Numeric scalar. PNG resolution in pixels per inch when
#'   `output_file` is supplied.
#' @param label_fun Function that accepts scale/label ids and returns display
#'   labels of the same length. Used only when `relabeling_panels = NULL`.
#' @param instrument_fun Function that accepts scale ids and returns display
#'   instrument labels of the same length. Used only when
#'   `relabeling_panels = NULL`.
#' @param fallacy_sum_text,jingle_fallacies_text,jangle_fallacies_text Optional
#'   named lists passed to `plot_relabeling_panel_a()`. Each list can contain
#'   `x`, `y`, `label`, `adj`, `cex`, `font`, and `col`.
#' @param parsimonious_solution_text,optimal_solution_text Optional named lists
#'   passed to `plot_relabeling_panel_a()`. These lists can also use `x_offset`
#'   to place text relative to the selected solution marker.
#' @param curve_legend,curve_legend_position,curve_legend_inset,curve_legend_cex
#'   Curve legend settings passed to `plot_relabeling_panel_a()`.
#' @param palette Character scalar. Palette used across the panels. Options are
#'   `"wulff_mata"` (default), `"cividis"`, `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"`, `"okabe_ito"`, and `"blue_gold"`.
#'   This single argument is passed to panels A, B, and C.
#' @param show_model_note Logical scalar. If `TRUE`, adds a small footnote below
#'   the figure identifying the model/solution used in panel A and panel C. The
#'   default is `FALSE` so package users opt in explicitly.
#' @param model_note Optional character scalar. Custom note to draw when
#'   `show_model_note = TRUE`. If `NULL`, the note is generated from
#'   `relabeling_panels`.
#' @param model_note_cex Numeric scalar. Character expansion for the model note.
#'
#' @return Invisibly returns `output_file`.
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
#' @examples
#' \dontrun{
#' plot_relabeling_figure(
#'   labels_scored = labels_scored,
#'   scale_label_cross = scale_label_cross,
#'   label_by_scale = label_by_scale,
#'   output_file = "04_relabeling.png",
#'   palette = "wulff_mata",
#'   fallacy_sum_text = list(x = 145, y = log(500)),
#'   jingle_fallacies_text = list(x = 210, y = log(300))
#' )
#' }
#' @export
plot_relabeling_figure <- function(
    labels_scored = NULL,
    scale_label_cross = NULL,
    label_by_scale = NULL,
    relabeling_panels = NULL,
    output_file = NULL,
    max_constructs = 100,
    solution = c("parsimonious", "optimal"),
    solution_type = NULL,
    width = 12,
    height = 15,
    res = 600,
    label_fun = scale_label_from_scale,
    instrument_fun = instrument_from_scale,
    fallacy_sum_text = NULL,
    jingle_fallacies_text = NULL,
    jangle_fallacies_text = NULL,
    parsimonious_solution_text = NULL,
    optimal_solution_text = NULL,
    curve_legend = TRUE,
    curve_legend_position = "topright",
    curve_legend_inset = .02,
    curve_legend_cex = .82,
    palette = "wulff_mata",
    show_model_note = FALSE,
    model_note = NULL,
    model_note_cex = .72) {

  solution <- match.arg(solution)

  if (is.null(relabeling_panels)) {
    relabeling_panels <- prepare_relabeling_panels(
      labels_scored = labels_scored,
      scale_label_cross = scale_label_cross,
      label_by_scale = label_by_scale,
      max_constructs = max_constructs,
      solution = solution,
      solution_type = solution_type,
      label_fun = label_fun,
      instrument_fun = instrument_fun,
      palette = palette
    )
  }

  if (!is.null(output_file)) {
    grDevices::png(output_file, width = width, height = height, res = res, units = "in")
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    if (!is.null(output_file)) grDevices::dev.off()
  }, add = TRUE)
  graphics::par(oma = c(if (isTRUE(show_model_note)) 2.1 else 0, 0, 0, 0))

  graphics::layout(matrix(c(1, 3, 2, 3), ncol = 2), width = c(.5, .5), height = c(.25, .8))
  plot_relabeling_panel_a(
    relabeling_panels$panel_a,
    fallacy_sum_text = fallacy_sum_text,
    jingle_fallacies_text = jingle_fallacies_text,
    jangle_fallacies_text = jangle_fallacies_text,
    parsimonious_solution_text = parsimonious_solution_text,
    optimal_solution_text = optimal_solution_text,
    curve_legend = curve_legend,
    curve_legend_position = curve_legend_position,
    curve_legend_inset = curve_legend_inset,
    curve_legend_cex = curve_legend_cex,
    palette = palette
  )
  plot_relabeling_panel_b(relabeling_panels$panel_b, palette = palette)
  plot_relabeling_panel_c(relabeling_panels$panel_c, palette = palette)

  if (isTRUE(show_model_note)) {
    if (is.null(model_note)) {
      model_note <- embeddcv_relabel_model_note(relabeling_panels)
    }
    graphics::mtext(model_note, side = 1, outer = TRUE, line = .55,
                    adj = .02, cex = model_note_cex, col = "grey35")
  }

  invisible(output_file)
}

embeddcv_relabel_text_spec <- function(default, override = NULL, anchor_x = NULL) {
  out <- default
  if (!is.null(override)) {
    out[names(override)] <- override
  }
  if (!is.null(out$x_offset) && !is.null(anchor_x)) {
    out$x <- anchor_x + out$x_offset
  }
  out
}

embeddcv_draw_relabel_text <- function(spec) {
  graphics::text(
    x = spec$x,
    y = spec$y,
    labels = spec$label,
    adj = if (is.null(spec$adj)) .5 else spec$adj,
    cex = if (is.null(spec$cex)) 1 else spec$cex,
    font = if (is.null(spec$font)) 1 else spec$font,
    col = if (is.null(spec$col)) "black" else spec$col
  )
}

embeddcv_relabel_type_label <- function(x) {
  labels <- c(
    hclust_single = "HC single",
    kmeans = "Kmeans",
    mclust = "Mclust",
    hclust_complete = "HC complete",
    hclust_ward = "HC ward",
    louvain = "Louvain",
    original_b5 = "Original taxonomy"
  )
  out <- labels[as.character(x)]
  out[is.na(out)] <- as.character(x)[is.na(out)]
  unname(out)
}

embeddcv_relabel_model_note <- function(relabeling_panels) {
  panel_c_solution <- relabeling_panels$panel_c$solution
  solution_kind <- relabeling_panels$panel_c$solution_kind
  if (is.null(solution_kind)) solution_kind <- "parsimonious"
  selection_scope <- relabeling_panels$panel_c$selection_scope
  if (is.null(selection_scope)) selection_scope <- "global"
  selection_label <- if (identical(selection_scope, "global")) "global " else ""
  panel_a_type <- relabeling_panels$panel_a$solution_type
  if (is.null(panel_a_type)) panel_a_type <- panel_c_solution$type

  paste0(
    "* Panel A shows the ",
    embeddcv_relabel_type_label(panel_a_type),
    " relabeling trajectory. Panel C shows the ",
    selection_label,
    solution_kind,
    " ",
    embeddcv_relabel_type_label(panel_c_solution$type),
    " solution (K = ",
    panel_c_solution$k,
    ", fallacies = ",
    panel_c_solution$fallacy_sum,
    ")."
  )
}

embeddcv_relabel_palette <- function(
    n,
    palette = c("wulff_mata", "cividis", "viridis", "magma", "plasma", "inferno", "okabe_ito", "blue_gold"),
    begin = 0,
    end = .9) {

  palette <- match.arg(palette)
  if (n <= 0) return(character())

  if (palette %in% c("wulff_mata", "cividis")) {
    return(viridisLite::cividis(n, begin = begin, end = end))
  }
  if (palette == "viridis") {
    return(viridisLite::viridis(n, begin = begin, end = end))
  }
  if (palette == "magma") {
    return(viridisLite::magma(n, begin = begin, end = end))
  }
  if (palette == "plasma") {
    return(viridisLite::plasma(n, begin = begin, end = end))
  }
  if (palette == "inferno") {
    return(viridisLite::inferno(n, begin = begin, end = end))
  }
  if (palette == "okabe_ito") {
    base <- c("#0072B2", "#E69F00", "#999999", "#009E73",
              "#D55E00", "#CC79A7", "#56B4E9", "#F0E442")
    return(rep(base, length.out = n))
  }

  grDevices::colorRampPalette(c("#08306B", "#8F9AA3", "#D8C35A"))(n)
}

embeddcv_in_pi <- function(x) (x / 360) * (pi * 2)
embeddcv_x_circ <- function(n, deg, rad, orig) {
  degs <- seq(0, deg, length = n + 1)
  cbind(degs, cos(embeddcv_in_pi(degs)) * rad + orig[1])
}
embeddcv_y_circ_p <- function(x, rad, orig) orig[2] + sqrt(abs(rad^2 - (x - orig[1])^2))
embeddcv_y_circ_m <- function(x, rad, orig) orig[2] - sqrt(abs(rad^2 - (x - orig[1])^2))
embeddcv_circle_raw <- function(n, deg, rad, orig) {
  xs <- embeddcv_x_circ(n, -deg, rad, orig)
  ys1 <- embeddcv_y_circ_p(xs[, 2], rad, orig)
  ys2 <- embeddcv_y_circ_m(xs[, 2], rad, orig)
  sel <- xs[, 1] > 180 & xs[, 1] < 360 |
    xs[, 1] > -180 & xs[, 1] < 0 |
    xs[, 1] > 640 & xs[, 1] < 720
  ys <- ys1
  ys[sel] <- ys2[sel]
  cbind(xs[, 2], ys)
}
embeddcv_circle_point <- function(deg, r = 1, orig = c(0, 0)) {
  embeddcv_circle_raw(1, -90 + deg, r, orig)[2, ]
}
embeddcv_rotate <- function(x, shift = 90) {
  x <- shift - x
  ifelse(x > 360, x - 360, x)
}
embeddcv_test_angle <- function(x) x <= 90 & x >= -90
embeddcv_rev_angle <- function(x) ifelse(x <= 90 & x >= -90, x, x - 180)
embeddcv_circular_mean_deg <- function(x) {
  radians <- x * pi / 180
  out <- atan2(mean(sin(radians)), mean(cos(radians))) * 180 / pi
  ifelse(out < 0, out + 360, out)
}
