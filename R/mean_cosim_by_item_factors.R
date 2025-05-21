
#' Compute Average Cosine Similarity by Item and Target Factors
#'
#' Takes a matrix of item-by-factors cosine similarities and, for each group of items (as defined by \code{item_factor}),
#' computes the mean Fisher-z-transformed cosine similarity to each target factor (columns of \code{cosim_mat}).
#' For each item factor, identifies both the highest and the second highest target factors (columns) by mean correlation.
#'
#' @param cosim_mat Numeric matrix [items x target_factors], e.g., cosine similarities.
#' @param item_factor Character or factor vector of length \code{nrow(cosim_mat)}, indicating the grouping of each item (e.g., facet or scale membership).
#' @param target_factor Character or factor vector of length \code{ncol(cosim_mat)}, indicating the name or label for each target factor (column).
#'
#' @return A list with:
#'   \describe{
#'     \item{matriz_corr}{A numeric matrix [length(unique(item_factor)) x length(unique(target_factor))] with mean Fisher-z-transformed correlations for each item factor by target factor.}
#'     \item{maior_corr_col}{A character vector [length(unique(target_factor))]: for each target factor, the item factor with the highest mean correlation.}
#'     \item{matches_by_row}{A data frame where each row corresponds to an item factor, with columns for the best and second best target factors and their respective correlation values.}
#'   }
#'
#' @details
#' The mean for each group is computed using Fisher-z transformation, then back-transformed to correlation scale.
#' The function is robust to missing values.
#'
#' @examples
#' # Example usage:
#' result <- mean_cosim_by_item_factors(
#'   cosim_mat = your_cosim_matrix,
#'   item_factor = your_item_facet_vector,
#'   target_factor = your_target_facet_vector
#' )
#' head(result$matches_by_row)
#'
#' @export

mean_cosim_by_item_factors <- function(cosim_mat, item_factor, target_factor) {
  unique_item_factors <- unique(item_factor)
  unique_target_factors <- unique(target_factor)
  n_factors_items <- length(unique_item_factors)
  n_factors_targets <- length(unique_target_factors)
  
  # Prepare output matrix and vector
  matriz_corr <- matrix(NA, nrow = n_factors_items, ncol = n_factors_targets)
  rownames(matriz_corr) <- unique_item_factors
  colnames(matriz_corr) <- unique_target_factors
  maior_corr_col <- character(n_factors_targets)
  
  # Main loop (columns)
  for (j in seq_along(unique_target_factors)) {
    col_factor <- unique_target_factors[j]
    media_corr <- numeric(n_factors_items)
    
    for (i in seq_along(unique_item_factors)) {
      fac <- unique_item_factors[i]
      logic_selector_item_factor <- item_factor == fac
      
      z_values <- 0.5 * log(
        (1 + cosim_mat[logic_selector_item_factor, col_factor]) /
          (1 - cosim_mat[logic_selector_item_factor, col_factor])
      )
      mean_z <- mean(z_values, na.rm = TRUE)
      media <- (exp(2 * mean_z) - 1) / (exp(2 * mean_z) + 1)
      media_corr[i] <- media
    }
    
    matriz_corr[, j] <- media_corr
    index_max_cor <- which.max(media_corr)
    maior_corr_col[j] <- unique_item_factors[index_max_cor]
  }
  names(maior_corr_col) <- unique_target_factors
  
  # For each item facet (row), get the best and second best target factors
  get_best_two <- function(x) {
    ord <- order(x, decreasing = TRUE)
    best1 <- ord[1]
    best2 <- if(length(ord) > 1) ord[2] else NA
    data.frame(
      best_target_factor = colnames(matriz_corr)[best1],
      best_correlation   = x[best1],
      second_best_target_factor = if (!is.na(best2)) colnames(matriz_corr)[best2] else NA,
      second_best_correlation   = if (!is.na(best2)) x[best2] else NA
    )
  }
  
  matches_by_row <- do.call(
    rbind, 
    apply(matriz_corr, 1, get_best_two)
  )
  
  matches_by_row <- cbind(
    item_factor = rownames(matriz_corr), 
    matches_by_row, 
    row.names = NULL
  )
  
  return(list(
    matriz_corr = matriz_corr,
    maior_corr_col = maior_corr_col,
    matches_by_row = matches_by_row
  ))
}
