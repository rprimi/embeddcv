# embedd_cv

## Overview

`embedd_cv` is an R package designed for analyzing psychological scales using embeddings and cosine similarity. It provides tools for generating text embeddings using OpenAI's API, computing cosine similarities between items and scales, and analyzing the relationships between items and factors using various complexity measures.

## Installation

You can install the development version of embedd_cv from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ricprimi/embedd_cv")
```

## Main Functions

The package provides three main functions:

1. `get_embeddings()`: Generate embeddings from text using OpenAI's API
2. `cosim_itens_scales()`: Compute cosine similarity matrix and complexity measures between items and scales
3. `mean_cosim_by_item_factors()`: Calculate average cosine similarity by item and target factors

## Usage

Here's a basic example of how to use the package:

```r
library(embedd_cv)

# Generate embeddings for items and scales
item_embeddings <- get_embeddings(
  api_key = "your_openai_api_key",
  text = c("Item 1 text", "Item 2 text"),
  model = "text-embedding-3-small"
)

scale_embeddings <- get_embeddings(
  api_key = "your_openai_api_key",
  text = c("Scale 1 description", "Scale 2 description"),
  model = "text-embedding-3-small"
)

# Compute cosine similarities and complexity measures
cosim_results <- cosim_itens_scales(
  item_emb = item_embeddings,
  scale_emb = scale_embeddings,
  item_text = c("Item 1 text", "Item 2 text"),
  factor_itens = c("Factor1", "Factor2"),
  factor_scale = c("Scale1", "Scale2")
)

# Analyze relationships between items and factors
factor_analysis <- mean_cosim_by_item_factors(
  cosim_mat = cosim_results[, c("Scale1", "Scale2")],
  item_factor = cosim_results$scale,
  target_factor = c("Scale1", "Scale2")
)
```

## Requirements

- R >= 3.5.0
- OpenAI API key for embedding generation
- Required R packages: httr, jsonlite, text2vec, dplyr, purrr

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
