#' Complete Workflow Example for embeddcv Package
#'
#' This example demonstrates a complete workflow using the embeddcv package,
#' from loading data to creating Sankey visualizations.
#'
#' @examples
#' \dontrun{
#' # Complete workflow example - to be implemented
#' # example_workflow()
#' }
#'
#' @export
example_workflow <- function() {
  # Load required libraries
  library(embeddcv)
  
  cat("Loading embeddcv package data...\n")
  
  # Load the BFI-2 and Facetmap data
  data("item_dic_bfi2", package = "embeddcv")
  data("item_dic_facetmap", package = "embeddcv")
  
  cat("BFI-2 items loaded:", nrow(item_dic_bfi2), "items\n")
  cat("Facetmap items loaded:", nrow(item_dic_facetmap), "items\n")
  
  # Display basic information about the datasets
  cat("\nBFI-2 domains available:\n")
  print(table(item_dic_bfi2$domain))
  
  cat("\nBFI-2 facets available:\n")
  print(table(item_dic_bfi2$domain_facet))
  
  cat("\nFacetmap key facets (first 10):\n")
  print(head(table(item_dic_facetmap$key_facet), 10))
  
  # Example 1: Creating sample embeddings and similarity matrix
  cat("\n=== Example 1: Sample Embeddings and Similarities ===\n")
  
  # For demonstration, create sample embeddings
  # In practice, these would be generated using get_embeddings() with real API
  set.seed(123)
  n_items <- 10
  n_scales <- 5
  embedding_dim <- 8
  
  # Sample embeddings for BFI-2 items
  item_embeddings <- matrix(runif(n_items * embedding_dim, -1, 1), 
                           nrow = n_items, ncol = embedding_dim)
  
  # Sample embeddings for Big Five domains
  scale_embeddings <- matrix(runif(n_scales * embedding_dim, -1, 1), 
                            nrow = n_scales, ncol = embedding_dim)
  
  # Use actual BFI-2 data for first 10 items
  item_texts <- item_dic_bfi2$item_en_text[1:n_items]
  item_factors <- item_dic_bfi2$domain[1:n_items]
  scale_factors <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
  
  cat("Computing cosine similarities and complexity measures...\n")
  
  # Compute cosine similarities and complexity measures
  cosim_results <- cosim_itens_scales(
    item_emb = item_embeddings,
    scale_emb = scale_embeddings,
    item_text = item_texts,
    factor_itens = item_factors,
    factor_scale = scale_factors
  )
  
  cat("Results summary:\n")
  print(summary(cosim_results[, scale_factors]))
  cat("Mean complexity:", mean(cosim_results$complexity), "\n")
  cat("Mean sparsity:", mean(cosim_results$sparsity), "\n")
  
  # Example 2: Analyze item-factor relationships
  cat("\n=== Example 2: Item-Factor Analysis ===\n")
  
  factor_analysis <- mean_cosim_by_item_factors(
    cosim_mat = as.matrix(cosim_results[, scale_factors]),
    item_factor = cosim_results$scale,
    target_factor = scale_factors
  )
  
  cat("Best matches by item factor:\n")
  print(factor_analysis$matches_by_row)
  
  cat("\nCorrelation matrix by factors:\n")
  print(round(factor_analysis$matriz_corr, 3))
  
  # Example 3: Create Sankey visualization
  cat("\n=== Example 3: Sankey Visualization ===\n")
  
  # Create a similarity matrix for visualization
  cosim_matrix <- as.matrix(cosim_results[, scale_factors])
  rownames(cosim_matrix) <- paste0("Item_", 1:n_items)
  
  cat("Creating Sankey diagram...\n")
  
  # Create interactive Sankey plot
  sankey_plot <- sankey_from_matrix(cosim_matrix, value = 0.3)
  
  cat("Sankey plot created! (Interactive plot will display if running in appropriate environment)\n")
  
  # Example 4: Working with Facetmap data
  cat("\n=== Example 4: Facetmap Data Exploration ===\n")
  
  # Show unique facets in facetmap data
  unique_facets <- unique(item_dic_facetmap$key_facet)
  cat("Number of unique facets in facetmap:", length(unique_facets), "\n")
  
  # Show distribution across Big Five domains
  cat("Distribution across Big Five domains:\n")
  print(table(item_dic_facetmap$Prim))
  
  # Example of working with specific facets
  absorption_items <- item_dic_facetmap[item_dic_facetmap$key_facet == "absorption", ]
  cat("\nNumber of absorption items:", nrow(absorption_items), "\n")
  cat("First absorption item:", substr(absorption_items$item_text[1], 1, 80), "...\n")
  
  cat("\n=== Workflow Complete ===\n")
  cat("This example demonstrates:\n")
  cat("1. Loading and exploring BFI-2 and Facetmap datasets\n")
  cat("2. Computing cosine similarities and complexity measures\n") 
  cat("3. Analyzing item-factor relationships\n")
  cat("4. Creating Sankey visualizations\n")
  cat("5. Working with psychological facet data\n")
  
  invisible(list(
    cosim_results = cosim_results,
    factor_analysis = factor_analysis,
    sankey_plot = sankey_plot,
    datasets = list(bfi2 = item_dic_bfi2, facetmap = item_dic_facetmap)
  ))
} 