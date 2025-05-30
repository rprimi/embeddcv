% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_cosim_by_item_factors.R
\name{mean_cosim_by_item_factors}
\alias{mean_cosim_by_item_factors}
\title{Compute Average Cosine Similarity by Item and Target Factors}
\usage{
mean_cosim_by_item_factors(cosim_mat, item_factor, target_factor)
}
\arguments{
\item{cosim_mat}{Numeric matrix \link{items x target_factors}, e.g., cosine similarities.}

\item{item_factor}{Character or factor vector of length \code{nrow(cosim_mat)}, indicating the grouping of each item (e.g., facet or scale membership).}

\item{target_factor}{Character or factor vector of length \code{ncol(cosim_mat)}, indicating the name or label for each target factor (column).}
}
\value{
A list with:
\describe{
\item{matriz_corr}{A numeric matrix \link{length(unique(item_factor)) x length(unique(target_factor))} with mean Fisher-z-transformed correlations for each item factor by target factor.}
\item{maior_corr_col}{A character vector \link{length(unique(target_factor))}: for each target factor, the item factor with the highest mean correlation.}
\item{matches_by_row}{A data frame where each row corresponds to an item factor, with columns for the best and second best target factors and their respective correlation values.}
}
}
\description{
Takes a matrix of item-by-factors cosine similarities and, for each group of items (as defined by \code{item_factor}),
computes the mean Fisher-z-transformed cosine similarity to each target factor (columns of \code{cosim_mat}).
For each item factor, identifies both the highest and the second highest target factors (columns) by mean correlation.
}
\details{
The mean for each group is computed using Fisher-z transformation, then back-transformed to correlation scale.
The function is robust to missing values.
}
\examples{
\dontrun{
# Create a sample cosine similarity matrix
# In practice, this would come from cosim_itens_scales()
set.seed(123)
n_items <- 20
n_targets <- 5
cosim_mat <- matrix(runif(n_items * n_targets, 0, 1), 
                    nrow = n_items, ncol = n_targets)

# Sample domain structure
item_factor <- sample(c("O_aes", "C_org", "E_soc", "A_trust", "N_anx"), n_items, replace = TRUE)
target_factor <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
colnames(cosim_mat) <- target_factor

# Compute mean similarities by factors
result <- mean_cosim_by_item_factors(
  cosim_mat = cosim_mat,
  item_factor = item_factor,
  target_factor = target_factor
)

# View results
head(result$matches_by_row)
result$matriz_corr
}

}
