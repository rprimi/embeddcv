#' BFI Items to Facetmap Scales Analysis Example
#'
#' This function demonstrates how to analyze BFI-2 items against Facetmap domain scales
#' using embeddings and cosine similarity, culminating in a Sankey visualization.
#'
#' @examples
#' \dontrun{
#' # Run the BFI to Facetmap analysis (requires package data)
#' # bfi_facetmap_example()
#' }
#'
#' @export
bfi_facetmap_example <- function() {
  
  # Load required libraries
  library(embeddcv)
  
  cat("=== BFI Items to Facetmap Scales Analysis ===\n")
  
  # Load the datasets
  cat("Loading datasets...\n")
  
  # The datasets should be available, let's load them directly from RDS files
  item_dic_bfi2 <- readRDS(system.file("data", "item_dic_bfi2.RDS", package = "embeddcv"))
  item_dic_facetmap <- readRDS(system.file("data", "item_dic_facetmap.RDS", package = "embeddcv"))
  
  cat("BFI-2 items loaded:", nrow(item_dic_bfi2), "items\n")
  cat("Facetmap items loaded:", nrow(item_dic_facetmap), "items\n")
  
  # Prepare BFI items (sources)
  cat("\n=== Preparing BFI Items (Sources) ===\n")
  bfi_items <- item_dic_bfi2$item_en_text[1:15]  # Use first 15 items for demonstration
  bfi_domains <- item_dic_bfi2$domain[1:15]
  bfi_facets <- item_dic_bfi2$domain_facet[1:15]
  
  cat("Selected", length(bfi_items), "BFI items:\n")
  cat("Domains:", paste(unique(bfi_domains), collapse = ", "), "\n")
  
  # Prepare Facetmap domain scales (targets) 
  cat("\n=== Preparing Facetmap Domain Scales (Targets) ===\n")
  facetmap_domains <- unique(item_dic_facetmap$Prim)
  facetmap_domain_descriptions <- c(
    "O" = "Openness to experience, creativity, imagination, and intellectual curiosity",
    "C" = "Conscientiousness, organization, discipline, and goal-directed behavior", 
    "E" = "Extraversion, sociability, assertiveness, and positive emotions",
    "A" = "Agreeableness, cooperation, trust, and concern for others",
    "N" = "Neuroticism, emotional instability, anxiety, and negative emotions"
  )
  
  domain_scale_names <- paste0("Domain_", names(facetmap_domain_descriptions))
  domain_scale_texts <- as.character(facetmap_domain_descriptions)
  
  cat("Target domain scales:", paste(domain_scale_names, collapse = ", "), "\n")
  
  # Create sample embeddings (in practice, use get_embeddings() with OpenAI API)
  cat("\n=== Creating Sample Embeddings ===\n")
  cat("Note: In practice, use get_embeddings() with your OpenAI API key\n")
  
  set.seed(123)
  embedding_dim <- 50
  
  # Sample embeddings for BFI items
  bfi_embeddings <- matrix(
    rnorm(length(bfi_items) * embedding_dim, 0, 1), 
    nrow = length(bfi_items), 
    ncol = embedding_dim
  )
  
  # Sample embeddings for domain scales  
  domain_embeddings <- matrix(
    rnorm(length(domain_scale_names) * embedding_dim, 0, 1),
    nrow = length(domain_scale_names),
    ncol = embedding_dim
  )
  
  cat("Created embeddings:", nrow(bfi_embeddings), "items x", ncol(bfi_embeddings), "dimensions\n")
  cat("Created embeddings:", nrow(domain_embeddings), "scales x", ncol(domain_embeddings), "dimensions\n")
  
  # Compute cosine similarities and complexity measures
  cat("\n=== Computing Cosine Similarities ===\n")
  
  cosim_results <- cosim_itens_scales(
    item_emb = bfi_embeddings,
    scale_emb = domain_embeddings,
    item_text = bfi_items,
    factor_itens = bfi_facets,
    factor_scale = domain_scale_names
  )
  
  cat("Similarity matrix computed. Summary statistics:\n")
  similarity_cols <- domain_scale_names
  print(summary(cosim_results[, similarity_cols]))
  
  cat("\nComplexity measures:\n")
  cat("Mean complexity:", round(mean(cosim_results$complexity), 3), "\n")
  cat("Mean sparsity:", round(mean(cosim_results$sparsity), 3), "\n")
  cat("Mean within-item SD:", round(mean(cosim_results$within_sd), 3), "\n")
  
  # Analyze factor relationships
  cat("\n=== Analyzing Factor Relationships ===\n")
  
  factor_analysis <- mean_cosim_by_item_factors(
    cosim_mat = as.matrix(cosim_results[, similarity_cols]),
    item_factor = cosim_results$scale,
    target_factor = domain_scale_names
  )
  
  cat("Best matches by BFI facet:\n")
  print(factor_analysis$matches_by_row)
  
  cat("\nMean correlation matrix (BFI facets x Domain scales):\n")
  print(round(factor_analysis$matriz_corr, 3))
  
  # Create Sankey visualization
  cat("\n=== Creating Sankey Visualization ===\n")
  
  # Prepare matrix for Sankey plot
  sankey_matrix <- as.matrix(cosim_results[, similarity_cols])
  rownames(sankey_matrix) <- paste0("BFI_", 1:nrow(sankey_matrix))
  colnames(sankey_matrix) <- gsub("Domain_", "", domain_scale_names)
  
  cat("Creating Sankey diagram with threshold = 0.4...\n")
  
  # Create the Sankey plot
  sankey_plot <- sankey_from_matrix(sankey_matrix, value = 0.4)
  
  cat("Sankey plot created successfully!\n")
  cat("Interactive plot will display in RStudio Viewer or browser if available.\n")
  
  # Summary and recommendations
  cat("\n=== Analysis Complete ===\n")
  cat("This example demonstrated:\n")
  cat("1. Loading BFI-2 items as sources\n")
  cat("2. Using Facetmap domain descriptions as targets\n") 
  cat("3. Computing cosine similarities between items and domains\n")
  cat("4. Analyzing complexity and sparsity measures\n")
  cat("5. Identifying best matches between BFI facets and domains\n")
  cat("6. Creating an interactive Sankey visualization\n")
  
  cat("\nFor real analysis:\n")
  cat("- Use get_embeddings() with your OpenAI API key\n")
  cat("- Consider using all BFI items, not just first 15\n")
  cat("- Experiment with different similarity thresholds\n")
  cat("- Try facet-level analysis instead of domain-level\n")
  
  # Return results for further analysis
  invisible(list(
    bfi_items = data.frame(
      text = bfi_items,
      domain = bfi_domains, 
      facet = bfi_facets
    ),
    domain_scales = data.frame(
      name = domain_scale_names,
      description = domain_scale_texts
    ),
    cosim_results = cosim_results,
    factor_analysis = factor_analysis,
    sankey_plot = sankey_plot
  ))
} 