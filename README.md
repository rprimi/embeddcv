# README


# embeddcv

## Overview

`embeddcv` is an R package designed for analyzing psychological scales
using embeddings and cosine similarity. It provides tools for generating
text embeddings using OpenAI’s API, computing cosine similarities
between items and scales, and analyzing the relationships between items
and factors using various complexity measures.

## Installation

You can install the development version of embeddcv from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rprimi/embeddcv")
devtools::load_all()
```

## Main Functions

The package provides four main functions:

1.  `get_embeddings()`: Generate embeddings from text using OpenAI’s API
2.  `cosim_itens_scales()`: Compute cosine similarity matrix and
    complexity measures between items and scales
3.  `mean_cosim_by_item_factors()`: Calculate average cosine similarity
    by item and target factors
4.  `sankey_from_matrix()`: Create interactive Sankey diagrams from
    similarity matrices

## Usage

Here’s a basic example of how to use the package with the included
datasets:

``` r
# Example: map BFI2 facets to the 70 facetmap scales 
# The workflow is as follows:
#
# 1. Generate embeddings for items and domain descriptions
# 2. Compute cosine similarities 
# 3. Analyze item-domain relationships
# 4. Visualize results with Sankey plots

library(embeddcv)

# Load the BFI-2 items data
 data("item_dic_bfi2", package = "embeddcv")
 data("item_dic_facetmap", package = "embeddcv")

# View the structure of BFI-2 items
 head(item_dic_bfi2)

# View the structure of Facetmap items
 head(item_dic_facetmap)

# Generate embeddings for BFI-2 items (requires OpenAI API key)
 
 bfi_item_embeddings <- get_embeddings(
   api_key = Sys.getenv("OPENAI_API_KEY"),
   text = item_dic_bfi2$item_en_text,
   model = "text-embedding-3-small"
 )

 facetmap_item_embeddings <- get_embeddings(
   api_key = Sys.getenv("OPENAI_API_KEY"),
   text = item_dic_facetmap$item_text,
   model = "text-embedding-3-small"
 )
 
# If you don't have an OpenAI API key load the saved embeddings from www.labape.com.br
 
 bfi_item_embeddings <- readRDS(url("http://www.labape.com.br/embeddcv/bfi_item_embeddings.RDS"))
 
 facetmap_item_embeddings <- readRDS(url("http://www.labape.com.br/embeddcv/facetmap_item_embeddings.RDS"))
 
# Compute avaregae embeddings for facet map scales
 library(dplyr)
 
 facetmap_scale_embeddings  <- facetmap_item_embeddings %>% 
  bind_cols(item_dic_facetmap[ , "domain_facet" ]) %>% 
  summarise(across(everything(), mean), .by = domain_facet)
 

 # Compute cosine similarities and complexity measures
  cosim_results <- cosim_itens_scales(
    item_emb =  bfi_item_embeddings,
    scale_emb = facetmap_scale_embeddings[ ,-1], # remove scale, column keep only embeddings
    item_text = item_dic_bfi2$item_en_text,
    factor_itens = item_dic_bfi2$domain_facet,
    factor_scale = facetmap_scale_embeddings$domain_facet
  )
 
  head(cosim_results)
  
# Compute mean cosine similarities per item-facet (rows of cosim) to the facetmap scales (columns of cosim )
  
   bfi_by_facets_summary <- mean_cosim_by_item_factors(
    cosim_mat = cosim_results[, 3:72], # keep only cosim
    item_factor =  cosim_results$scale,
    target_factor = names(cosim_results[, 3:72])
   )
  
# Chek results: BFI-2 facets by facetmap cosine similarities
  head(bfi_by_facets_summary$matriz_corr) 

# Chek results: Top 2 facetmap scales matched to BFI-2 facets  
  head(bfi_by_facets_summary$matches_by_row) 
 
   
# Create a Sankey plot from a cosine similarity matrix 
# Experiment varying the values to see more linked facets (lower values) or less (higher values)
  sankey_from_matrix( bfi_by_facets_summary$matriz_corr , value = 0.38)
  
  
# Save results to excel
 library(xlsx)
 
 cosim_results %>% 
  select(item_text, scale, complexity, sparsity,  within_sd, everything())  %>% 
  write.xlsx(file = "results.xlsx", showNA = F, append = T, sheetName = "cosim_mat")

 bfi_by_facets_summary$matriz_corr %>% write.xlsx(
  file = "results.xlsx", showNA = F, append = T, sheetName = "bfi2_facets_by_facemap")

  bfi_by_facets_summary$matches_by_row %>% write.xlsx(
  file = "results.xlsx", showNA = F, append = T, sheetName = "best_matches_aspects")
```

\`\`\`

\`\`\`r

## Requirements

- R \>= 3.5.0
- OpenAI API key for embedding generation
- Required R packages: httr, jsonlite, text2vec, dplyr, purrr, networkD3

## License

This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details.
