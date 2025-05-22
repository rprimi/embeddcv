# embeddcv

## Overview

`embeddcv` is an R package designed for analyzing psychological scales using embeddings and cosine similarity. It provides tools for generating text embeddings using OpenAI's API, computing cosine similarities between items and scales, and analyzing the relationships between items and factors using various complexity measures.

## Installation

You can install the development version of embeddcv from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ricprimi/embeddcv")
```

## Main Functions

The package provides four main functions:

1. `get_embeddings()`: Generate embeddings from text using OpenAI's API
2. `cosim_itens_scales()`: Compute cosine similarity matrix and complexity measures between items and scales
3. `mean_cosim_by_item_factors()`: Calculate average cosine similarity by item and target factors
4. `sankey_from_matrix()`: Create interactive Sankey diagrams from similarity matrices

## Usage

Here's a basic example of how to use the package with the included datasets:

```r
library(embeddcv)

# Load the BFI-2 items data
data("item_dic_bfi2", package = "embeddcv")
data("item_dic_facetmap", package = "embeddcv")

# View the structure of BFI-2 items
head(item_dic_bfi2)

# View the structure of Facetmap items
head(item_dic_facetmap)

# Example: Generate embeddings for BFI-2 items (requires OpenAI API key)
# item_embeddings <- get_embeddings(
#   api_key = "your_openai_api_key",
#   text = item_dic_bfi2$item_en_text[1:5],
#   model = "text-embedding-3-small"
# )

# Example: Create a Sankey plot from a cosine similarity matrix
# Create a sample matrix for demonstration
set.seed(123)
cosim_matrix <- matrix(runif(25, 0, 1), nrow = 5, ncol = 5)
rownames(cosim_matrix) <- item_dic_bfi2$coditem[1:5]
colnames(cosim_matrix) <- unique(item_dic_bfi2$domain_facet)[1:5]

# Create interactive Sankey diagram
sankey_from_matrix(cosim_matrix, value = 0.4)

# Example workflow with actual data:
# 1. Generate embeddings for items and domain descriptions
# 2. Compute cosine similarities 
# 3. Analyze item-domain relationships
# 4. Visualize results with Sankey plots
```

## Requirements

- R >= 3.5.0
- OpenAI API key for embedding generation
- Required R packages: httr, jsonlite, text2vec, dplyr, purrr, networkD3

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
