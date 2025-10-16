# embeddcv <img src="man/figures/logo_1.png" align="right" height="139" alt="" />

## 📊 embeddcv: Analyzing Psychological Scales with Embeddings and Cosine Similarity

**embeddcv** is an R package that leverages modern embedding techniques to analyze psychological scales and their relationships. It provides a comprehensive toolkit for generating text embeddings, computing cosine similarities between items and scales, and visualizing complex relationships through interactive diagrams.

## ✨ Features

- 🔤 **Multi-Provider Embedding Support**: Generate embeddings using OpenAI, Google Gemini, or HuggingFace APIs
- 📐 **Cosine Similarity Analysis**: Compute similarity matrices between items and scales
- 📊 **Complexity Measures**: Analyze item-factor relationships using various complexity metrics
- 🎯 **Factor Matching**: Identify best-matching scales for psychological items
- 📈 **Interactive Visualizations**: Create Sankey diagrams for exploring similarity patterns

## 📦 Installation

### Development Version

Install the latest development version from GitHub:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install embeddcv from GitHub
remotes::install_github("rprimi/embeddcv")
```

### Dependencies

The package requires the following R packages:
- `dplyr` (>= 1.0.0)
- `httr` (>= 1.4.0)
- `jsonlite` (>= 1.7.0)
- `networkD3` (>= 0.4)
- `purrr` (>= 0.3.0)
- `text2vec` (>= 0.6.0)

## 🚀 Quick Start

```r
library(embeddcv)

# Load example datasets
data("item_dic_bfi2", package = "embeddcv")
data("item_dic_facetmap", package = "embeddcv")

# Generate embeddings (requires API key)
embeddings <- get_embeddings(
  text = item_dic_bfi2$item_en_text,
  provider = "openai",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  model = "text-embedding-3-small"
)
```

## 📖 Core Functions

### 1. `get_embeddings()`
Generate embeddings from text using various providers.

```r
# OpenAI example
embeddings <- get_embeddings(
  text = your_text_vector,
  provider = "openai",
  api_key = your_api_key,
  model = "text-embedding-3-small"
)

# Google Gemini example
embeddings <- get_embeddings(
  text = your_text_vector,
  provider = "google",
  api_key = your_api_key,
  model = "embedding-001"
)

# HuggingFace example
embeddings <- get_embeddings(
  text = your_text_vector,
  provider = "huggingface",
  api_key = your_api_key,
  model = "sentence-transformers/all-MiniLM-L6-v2"
)
```

### 2. `cosim_itens_scales()`
Compute cosine similarity matrix and complexity measures.

```r
cosim_results <- cosim_itens_scales(
  item_emb = item_embeddings,
  scale_emb = scale_embeddings,
  item_text = item_descriptions,
  factor_itens = item_factors,
  factor_scale = scale_factors
)
```

### 3. `mean_cosim_by_item_factors()`
Calculate average cosine similarity by item and target factors.

```r
summary_results <- mean_cosim_by_item_factors(
  cosim_mat = cosim_matrix,
  item_factor = item_factors,
  target_factor = target_factors
)
```

### 4. `sankey_from_matrix()`
Create interactive Sankey diagrams from similarity matrices.

```r
sankey_from_matrix(
  similarity_matrix, 
  value = 0.38  # Threshold for connections
)
```

## 📚 Complete Example: Mapping BFI-2 to Facetmap Scales

This example demonstrates the complete workflow for mapping BFI-2 personality facets to the 70 Facetmap scales:

```r
library(embeddcv)

# Step 1: Load datasets
data("item_dic_bfi2", package = "embeddcv")
data("item_dic_facetmap", package = "embeddcv")

# Step 2: Generate embeddings
## Option A: Use API (requires API key)
bfi_embeddings <- get_embeddings(
  text = item_dic_bfi2$item_en_text,
  provider = "openai",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  model = "text-embedding-3-small"
)

## Option B: Load pre-computed embeddings (for demo)
bfi_embeddings <- readRDS(
  url("http://www.labape.com.br/embeddcv/bfi_item_embeddings.RDS")
)
facetmap_embeddings <- readRDS(
  url("http://www.labape.com.br/embeddcv/facetmap_item_embeddings.RDS")
)

# Step 3: Compute scale-level embeddings
domain_facet <- unique(item_dic_facetmap$domain_facet)
facetmap_scale_embeddings <- data.frame(
  domain_facet,
  t(sapply(domain_facet, function(g) {
    colMeans(facetmap_embeddings[item_dic_facetmap$domain_facet == g, ])
  }))
)

# Step 4: Calculate cosine similarities
cosim_results <- cosim_itens_scales(
  item_emb = bfi_embeddings,
  scale_emb = facetmap_scale_embeddings[, -1],
  item_text = item_dic_bfi2$item_en_text,
  factor_itens = item_dic_bfi2$domain_facet,
  factor_scale = facetmap_scale_embeddings$domain_facet
)

# Step 5: Analyze relationships
keepCosim <- grep("item_text|scale|complexity|sparsity|within_sd", 
                  colnames(cosim_results))
summary_results <- mean_cosim_by_item_factors(
  cosim_mat = cosim_results[, -keepCosim],
  item_factor = cosim_results$scale,
  target_factor = names(cosim_results[, -keepCosim])
)

# Step 6: Visualize with Sankey diagram
sankey_from_matrix(summary_results$matriz_corr, value = 0.38)
```

## 🔑 API Configuration

### Setting up API Keys

Store your API keys as environment variables for security:

```r
# Add to your .Renviron file
OPENAI_API_KEY=your_openai_key_here
GEMINI_API_KEY=your_google_key_here
HF_API_TOKEN=your_huggingface_token_here

# Or set temporarily in R session
Sys.setenv(OPENAI_API_KEY = "your_key_here")
```

### Supported Embedding Models

| Provider | Models | Documentation |
|----------|--------|---------------|
| OpenAI | `text-embedding-3-small`, `text-embedding-3-large`, `text-embedding-ada-002` | [OpenAI Embeddings](https://platform.openai.com/docs/guides/embeddings) |
| Google Gemini | `embedding-001`, `text-embedding-004` | [Gemini API](https://ai.google.dev/tutorials/python_quickstart) |
| HuggingFace | Various Sentence Transformers models | [HuggingFace Models](https://huggingface.co/models) |

## 📊 Example Datasets

The package includes two example datasets:

- **`item_dic_bfi2`**: BFI-2 personality inventory items with English text and factor mappings
- **`item_dic_facetmap`**: Facetmap scale items with 70 personality facets

```r
# Explore the datasets
data("item_dic_bfi2", package = "embeddcv")
head(item_dic_bfi2)

data("item_dic_facetmap", package = "embeddcv")
head(item_dic_facetmap)
```

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📄 Citation

If you use `embeddcv` in your research, please cite:

```bibtex
@software{embeddcv2024,
  title = {embeddcv: Analyzing Psychological Scales with Embeddings},
  author = {Primi, R.},
  year = {2024},
  url = {https://github.com/rprimi/embeddcv}
}
```

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


## 📧 Contact

- **Author**: R. Primi
- **Repository**: [GitHub](https://github.com/rprimi/embeddcv)
- **Issues**: [Report a bug](https://github.com/rprimi/embeddcv/issues)
- **Website**: [LabAPE](http://www.labape.com.br)

---
