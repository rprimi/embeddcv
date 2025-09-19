
#' Compute Cosine Similarity Matrix and Complexity Measures between Items and Scales
#'
#' Calculates the cosine similarity between item embeddings and scale embeddings,
#' then computes Hoffman’s Complexity Index, Hoyer’s Sparsity Index, and within-row standard deviation
#' for each item, summarizing how closely each item relates to all scales.
#'
#' @param item_emb Numeric matrix or data frame. Embedding vectors for items (rows = items, cols = embedding dimensions).
#' @param scale_emb Numeric matrix or data frame. Embedding vectors for scales (rows = scales, cols = embedding dimensions).
#' @param item_text Character vector. Textual content or labels for each item (length = number of rows in \code{item_emb}).
#' @param factor_itens Character or factor vector. Factor or group label for each item (length = number of rows in \code{item_emb}).
#' @param factor_scale Character or factor vector. Names of scales (length = number of rows in \code{scale_emb}).
#'
#' @return A data frame with one row per item, containing:
#'   \describe{
#'     \item{item_text}{Original text or label for each item.}
#'     \item{scale}{Factor or group label for each item.}
#'     \item{<scale columns>}{Cosine similarity of each item to each scale.}
#'     \item{complexity}{Hoffman's Complexity Index for each item's cosine similarity profile.}
#'     \item{sparsity}{Hoyer's Sparsity Index for each item's similarity profile.}
#'     \item{within_sd}{Standard deviation of the cosine similarities for each item (row-wise).}
#'   }
#'
#' @details
#' The cosine similarity matrix is computed using \code{text2vec::sim2} (method = "cosine", norm = "l2").
#' Hoffman’s Complexity Index quantifies the degree of simple vs. complex structure in each item's profile;
#' Hoyer’s Sparsity Index quantifies how close the profile is to being sparse (single dominant value).
#'
#' @import text2vec
#' @importFrom dplyr bind_cols
#'
#' @examples
#' \dontrun{
#' # Create sample embeddings for demonstration
#' # In practice, these would be generated using get_embeddings()
#' set.seed(123)
#' item_emb <- matrix(runif(60, -1, 1), nrow = 6, ncol = 10)  # 6 items, 10 dimensions
#' scale_emb <- matrix(runif(50, -1, 1), nrow = 5, ncol = 10)  # 5 scales, 10 dimensions
#'
#' # Sample item texts and factors
#' item_text <- c("I am outgoing", "I worry", "I like art", "I help others", "I am organized", "I think deeply")
#' factor_itens <- c("E", "N", "O", "A", "C", "O")
#' factor_scale <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
#'
#' # Compute cosine similarities and complexity measures
#' result <- cosim_itens_scales(item_emb, scale_emb, item_text, factor_itens, factor_scale)
#' head(result)
#' }
#'
#' @export
cosim_itens_scales <- function(
    item_emb,
    scale_emb,
    item_text,
    factor_itens,
    factor_scale){

  m1 <- as.matrix(item_emb)
  m2 <- as.matrix(scale_emb)

  cosim_mat <- as.data.frame(sim2(
    x      = m1,
    y      = m2,
    method = "cosine",
    norm   = "l2"
  ))
  colnames(cosim_mat) <- factor_scale

  # Calcula Hoyer's Sparsity e Hoffman's _Complexity
  cosim_mat$complexity = apply(cosim_mat[ , factor_scale], MARGIN = 1, FUN = hoffman_complexity)
  cosim_mat$sparsity = apply(cosim_mat[ , factor_scale], MARGIN = 1, FUN = hoyer_sparsity)
  cosim_mat$within_sd = apply(cosim_mat[ , factor_scale], MARGIN = 1, FUN = sd)

  cosim_mat <- bind_cols(
    item_text = item_text,
    scale = factor_itens,
    cosim_mat
  )

  return(cosim_mat)
}

#' Hoyer's Sparsity Measure
#'
#' Computes Hoyer's sparsity metric, which quantifies the level of sparsity in a numeric vector.
#' The value ranges from 0 (completely dense) to 1 (maximally sparse).
#'
#' @param x A numeric vector.
#'
#' @return A numeric value between 0 and 1 representing the sparsity.
#' Returns 0 if the \eqn{L_2} norm of \code{x} is zero to avoid division by zero.
#'
#' @details
#' Hoyer's sparsity is defined as:
#' \deqn{ \frac{\sqrt{n} - \frac{||x||_1}{||x||_2}}{\sqrt{n} - 1} }
#' where \eqn{n} is the length of the vector, \eqn{||x||_1} is the L1 norm,
#' and \eqn{||x||_2} is the L2 norm.
#'
#' @examples
#' hoyer_sparsity(c(1, 0, 0, 0))
#' hoyer_sparsity(rep(1, 4))
#'
#' @export
hoyer_sparsity <- function(x) {
  n <- length(x)
  l1_norm <- sum(abs(x))
  l2_norm <- sqrt(sum(x^2))
  if (l2_norm == 0) return(0)  # Avoid division by zero
  sparsity <- (sqrt(n) - (l1_norm / l2_norm)) / (sqrt(n) - 1)
  return(sparsity)
}


#' Hoffman's Complexity Index
#'
#' Computes Hoffman's complexity index for a numeric vector, often used for factor loadings or correlation patterns.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing the complexity. Returns \code{NA} if the sum of the fourth powers is zero to avoid division by zero.
#'
#' @details
#' Hoffman's complexity index is defined as:
#' \deqn{ \frac{(\sum x^2)^2}{\sum x^4} }
#' This measure captures the spread or d
hoffman_complexity <- function(x) {
  # x: numeric vector (e.g., loadings or correlations)
  sum_sq <- sum(x^2)
  sum_quad <- sum(x^4)
  if (sum_quad == 0) return(NA) # avoid division by zero
  complexity <- (sum_sq^2) / sum_quad
  return(complexity)
}


