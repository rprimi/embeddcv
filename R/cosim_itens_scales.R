
#' Compute Cosine Similarity Matrix and Complexity Measures between Items and Scales
#'
#' Calculates the cosine similarity between item embeddings and scale embeddings,
#' then computes Hoffman's Complexity Index, Hoyer's Sparsity Index, and within-row standard deviation
#' for each item, summarizing how closely each item relates to all scales.
#' Also returns a distribution plot of all cosine similarity coefficients as a
#' benchmarking utility.
#'
#' @param item_emb Numeric matrix or data frame. Embedding vectors for items (rows = items, cols = embedding dimensions).
#' @param scale_emb Numeric matrix or data frame. Embedding vectors for scales (rows = scales, cols = embedding dimensions).
#' @param item_text Character vector. Textual content or labels for each item (length = number of rows in \code{item_emb}).
#' @param factor_itens Character or factor vector. Factor or group label for each item (length = number of rows in \code{item_emb}).
#' @param factor_scale Character or factor vector. Names of scales (length = number of rows in \code{scale_emb}).
#'
#' @return A list with:
#'   \describe{
#'     \item{cosim_mat}{A data frame with one row per item, containing:
#'       \code{item_text}, \code{scale}, one column per scale with cosine similarities,
#'       \code{best_target_factor}, \code{second_best_target_factor},
#'       \code{complexity}, \code{sparsity}, and \code{within_sd}.}
#'     \item{plot_dist}{A ggplot histogram of all cosine similarity values (pooled across
#'       scales), with decile reference lines and a density overlay. Use this to benchmark
#'       the empirical magnitude of cosim coefficients for your embedding model and item set.}
#'   }
#'
#' @details
#' The cosine similarity matrix is computed using \code{text2vec::sim2} (method = "cosine", norm = "l2").
#' Hoffman's Complexity Index quantifies the degree of simple vs. complex structure in each item's profile;
#' Hoyer's Sparsity Index quantifies how close the profile is to being sparse (single dominant value).
#'
#' @import text2vec
#' @importFrom dplyr bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline annotate
#'   scale_x_continuous scale_y_continuous labs theme_minimal after_stat
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' item_emb  <- matrix(runif(60, -1, 1), nrow = 6, ncol = 10)
#' scale_emb <- matrix(runif(50, -1, 1), nrow = 5, ncol = 10)
#' item_text    <- c("I am outgoing", "I worry", "I like art", "I help others", "I am organized", "I think deeply")
#' factor_itens <- c("E", "N", "O", "A", "C", "O")
#' factor_scale <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
#'
#' result <- cosim_itens_scales(item_emb, scale_emb, item_text, factor_itens, factor_scale)
#' head(result$cosim_mat)
#' result$plot_dist
#' }
#'
#' @export
cosim_itens_scales <- function(
    item_emb,
    scale_emb,
    item_text,
    factor_itens,
    factor_scale) {

  m1 <- as.matrix(item_emb)
  m2 <- as.matrix(scale_emb)

  cosim_mat <- as.data.frame(sim2(
    x      = m1,
    y      = m2,
    method = "cosine",
    norm   = "l2"
  ))
  colnames(cosim_mat) <- factor_scale

  # Best and second best scale per item
  best_two <- t(apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = function(x) {
    ord <- order(x, decreasing = TRUE)
    c(best_target_factor          = factor_scale[ord[1]],
      second_best_target_factor   = if (length(ord) > 1) factor_scale[ord[2]] else NA_character_)
  }))
  cosim_mat$best_target_factor        <- best_two[, "best_target_factor"]
  cosim_mat$second_best_target_factor <- best_two[, "second_best_target_factor"]

  cosim_mat$complexity <- apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = hoffman_complexity)
  cosim_mat$sparsity   <- apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = hoyer_sparsity)
  cosim_mat$within_sd  <- apply(cosim_mat[, factor_scale], MARGIN = 1, FUN = sd)

  cosim_mat <- bind_cols(
    item_text = item_text,
    scale     = factor_itens,
    cosim_mat
  )

  # ── Distribution plot ─────────────────────────────────────────────────────────
  long <- tidyr::pivot_longer(
    cosim_mat[, factor_scale],
    cols      = tidyr::everything(),
    names_to  = "scale_name",
    values_to = "cosim"
  )

  pct        <- quantile(long$cosim, probs = seq(.1, .9, .1), na.rm = TRUE)
  mean_val   <- mean(long$cosim, na.rm = TRUE)
  sd_val     <- sd(long$cosim,   na.rm = TRUE)
  n_vals     <- sum(!is.na(long$cosim))
  subtitle   <- sprintf("n = %d  |  mean = %.3f  |  sd = %.3f  |  range = [%.3f, %.3f]",
                        n_vals, mean_val, sd_val,
                        min(long$cosim, na.rm = TRUE),
                        max(long$cosim, na.rm = TRUE))

  plot_dist <- ggplot2::ggplot(long, ggplot2::aes(x = cosim)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      binwidth = 0.02,
      colour   = "white",
      fill     = "salmon",
      alpha    = 0.85
    ) +
    ggplot2::geom_density(
      colour    = "#c0392b",
      linewidth = 0.8,
      adjust    = 1.2
    ) +
    ggplot2::geom_vline(
      xintercept = pct,
      linetype   = "dashed",
      colour     = "grey30",
      linewidth  = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val,
      linetype   = "solid",
      colour     = "#2c3e50",
      linewidth  = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x      = pct,
      y      = Inf,
      label  = names(pct),
      angle  = 90,
      vjust  = -0.4,
      hjust  = 1.1,
      size   = 2.8,
      colour = "grey30"
    ) +
    ggplot2::annotate(
      "text",
      x      = mean_val,
      y      = Inf,
      label  = paste0("mean\n", round(mean_val, 3)),
      vjust  = -0.3,
      hjust  = -0.15,
      size   = 2.8,
      colour = "#2c3e50"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(0.10)) +
    ggplot2::labs(
      title    = "Distribution of Cosine Similarity Coefficients",
      subtitle = subtitle,
      x        = "Cosine similarity",
      y        = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  list(
    cosim_mat = cosim_mat,
    plot_dist = plot_dist
  )
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
  if (l2_norm == 0) return(0)
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
#' This measure captures the spread or dominance of loadings across dimensions.
#'
#' @export
hoffman_complexity <- function(x) {
  sum_sq   <- sum(x^2)
  sum_quad <- sum(x^4)
  if (sum_quad == 0) return(NA)
  (sum_sq^2) / sum_quad
}
