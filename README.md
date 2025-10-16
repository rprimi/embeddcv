# embeddcv

## Overview

`embeddcv` is an R package designed for analyzing psychological scales
using embeddings and cosine similarity. It provides tools for generating
embeddings from text, computing cosine similarities items and scales,
and analyzing the relationships between items and factors using various
complexity measures.

## Installation

You can install the development version of `embeddcv` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rprimi/embeddcv")
```

## Main Functions

The package provides four main functions:

1.  `get_embeddings()`: Generate embeddings from text
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

### Load the package====
#' This package allows the user to use OpenAI API, Google Gemini API, or
#' HuggingFace API to extract embeddings for a user-provided text.
#' The unified get_embeddings() function supports all three providers.
library(embeddcv)

### Load the BFI-2 items data====
# Items to be analyzed
data("item_dic_bfi2", package = "embeddcv")
# Domain descriptions
data("item_dic_facetmap", package = "embeddcv")
# View the structure of BFI-2 items
head(item_dic_bfi2)
# View the structure of Facetmap items
head(item_dic_facetmap)

### Step 1. Generate embeddings for items and domain descriptions====
## If you have an OpenAI API key, use the function as this:
#bfi_item_embeddings_api <- get_embeddings(
#  text = item_dic_bfi2$item_en_text,
#  provider = "openai",
#  api_key = Sys.getenv("OPENAI_API_KEY"),
#  model = "text-embedding-3-small"
#)
#facetmap_item_embeddings_api <- get_embeddings(
#  text = item_dic_facetmap$item_text,
#  provider = "openai",
#  api_key = Sys.getenv("OPENAI_API_KEY"),
#  model = "text-embedding-3-small"
#)

## If you have a Google API key, use the function as this:
#bfi_item_embeddings_google <- get_embeddings(
#  text = item_dic_bfi2$item_en_text,
#  provider = "google",
#  api_key = Sys.getenv("GEMINI_API_KEY"),
#  model = "embedding-001"
#)

## If you have a HuggingFace API token, use the function as this:
#bfi_item_embeddings_hf <- get_embeddings(
#  text = item_dic_bfi2$item_en_text,
#  provider = "huggingface",
#  api_key = Sys.getenv("HF_API_TOKEN"),
#  model = "sentence-transformers/all-MiniLM-L6-v2"
#)

## Otherwise, for this example, you can download the saved OpenAI
## embeddings from www.labape.com.br
bfi_item_embeddings_api <- readRDS(url("http://www.labape.com.br/embeddcv/bfi_item_embeddings.RDS"))
facetmap_item_embeddings_api <- readRDS(url("http://www.labape.com.br/embeddcv/facetmap_item_embeddings.RDS"))

## Compute average embeddings for facet map scales
# Identify the specific facets
domain_facet <- unlist(c(unique(item_dic_facetmap[ , "domain_facet" ])))
# Average embeddings for OpenAI embeddings
facetmap_scale_embeddings_api <- data.frame(domain_facet, t(sapply(domain_facet, function(g) {
  colMeans(facetmap_item_embeddings_api[item_dic_facetmap[,"domain_facet"]==g,])
})))
# Average embeddings for OpenAI embeddings
facetmap_scale_embeddings_free <- data.frame(domain_facet, t(sapply(domain_facet, function(g) {
  colMeans(facetmap_item_embeddings_free[item_dic_facetmap[,"domain_facet"]==g,])
})))

### Step 2. Compute cosine similarities====
# Compute cosine similarities and complexity measures for OpenAI embeddings
cosim_results_api <- cosim_itens_scales(
  item_emb =  bfi_item_embeddings_api,
  scale_emb = facetmap_scale_embeddings_api[,-1], # remove scale column, keep only embeddings
  item_text = item_dic_bfi2$item_en_text,
  factor_itens = item_dic_bfi2$domain_facet,
  factor_scale = facetmap_scale_embeddings_api$domain_facet
)
# Compute cosine similarities and complexity measures for OpenAI embeddings
cosim_results_free <- cosim_itens_scales(
  item_emb =  bfi_item_embeddings_free,
  scale_emb = facetmap_scale_embeddings_free[,-1], # remove scale column, keep only embeddings
  item_text = item_dic_bfi2$item_en_text,
  factor_itens = item_dic_bfi2$domain_facet,
  factor_scale = facetmap_scale_embeddings_api$domain_facet
)

### Step 3. Analyze item-domain relationships====
#' Compute mean cosine similarities per item-facet (rows of cosim)
#' to the facetmap scales(columns of cosim )
# Select columns that will not be used in the analysis
keepCosim <- grep("item_text|scale|complexity|sparsity|within_sd", colnames(cosim_results_free))
# Mean cosine similarities for Open AI API
bfi_by_facets_summary_api <- mean_cosim_by_item_factors(
  cosim_mat = cosim_results_api[,-keepCosim], # keep only cosim
  item_factor =  cosim_results_api$scale,
  target_factor = names(cosim_results_api[,-keepCosim])
)
# Mean cosine similarities for Open AI API
bfi_by_facets_summary_free <- mean_cosim_by_item_factors(
  cosim_mat = cosim_results_free[,-keepCosim], # keep only cosim
  item_factor =  cosim_results_free$scale,
  target_factor = names(cosim_results_free[,-keepCosim])
)
# Check results: Top 2 facetmap scales matched to BFI-2 facets Open AI API
head(bfi_by_facets_summary_api$matches_by_row)
# Check results: Top 2 facetmap scales matched to BFI-2 facets Free model
head(bfi_by_facets_summary_free$matches_by_row)

### Step 4. Visualize results with Sankey plots====
## Create a Sankey plot from a cosine similarity matrix
#' Experiment varying the values to see more linked facets (lower values)
#' or less (higher values)
# Open AI API Sankey Plot
sankey_from_matrix(bfi_by_facets_summary_api$matriz_corr, value = 0.38)
# Free LLM Sankey Plot
sankey_from_matrix(bfi_by_facets_summary_free$matriz_corr, value = 0.68)

####====---- THE END ----====####
```


## Requirements

- R \>= 4.5.1
- API key for embedding generation (OpenAI, Google Gemini, or HuggingFace)
- Required R packages: dplyr, httr, jsonlite, networkD3, purrr, text2vec

## License

This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details.
