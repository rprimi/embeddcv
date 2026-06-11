# embeddcv <img src="man/figures/logo_1.png" align="right" height="139" alt="" />

## 📊 embeddcv: Analyzing Psychological Scales with Embeddings and Cosine Similarity

**embeddcv** is an R package that leverages modern embedding techniques to analyze psychological scales and their relationships.
It provides a comprehensive toolkit for generating text embeddings, computing cosine similarities between items and scales, 
and visualizing complex relationships through interactive diagrams. This package also implements the jingle-jangle detection method introduced by Wulff & Mata (2025), 
providing a convenient wrapper around the functions open-sourced by the authors. The method uses large language model embeddings 
to identify jingle fallacies (identical labels referring to different constructs) and jangle fallacies (different labels referring to the same construct),
helping increase conceptual clarity in psychological measurement.

Reference: Wulff, D. U., & Mata, R. (2025). Escaping the jingle-jangle jungle: Increasing conceptual clarity in psychology using large language models. Nature Human Behaviour, 9, 583–593. https://doi.org/10.1038/s41562-024-02089-y)


## ✨ Features

- 🔤 **Multi-Provider Embedding Support**: Generate embeddings using OpenAI, Google Gemini, or HuggingFace APIs
- 📐 **Cosine Similarity Analysis**: Compute similarity matrices between items and scales
- 📊 **Complexity Measures**: Analyze item-factor relationships using various complexity metrics
- 🎯 **Factor Matching**: Identify best-matching scales for psychological items
- 📈 **Interactive Visualizations**: Create Sankey diagrams for exploring similarity patterns
- 📄 **jingle–jangle detection**: Wraps the open-source code from Wulff & Mata (2025) for jingle-jangle analysis and visualization.

### Development Version

Install the latest development version from GitHub:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install embeddcv from GitHub
remotes::install_github("rprimi/embeddcv")
```

### Dependencies

The main R packages used are `text2vec`, `dplyr`, `tidyr`, `ggplot2`, `httr`,
`jsonlite`, `networkD3`, `plotly`, `psych`, and `reticulate` (for local
HuggingFace models). See the `DESCRIPTION` file for the complete list —
all are installed automatically by `remotes::install_github()`.

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
Compute cosine similarity matrix and complexity measures. Returns a list:
`$cosim_mat` (wide data frame with one cosine column per scale plus
complexity/sparsity measures), `$cosim_long` (long pair table),
`$cosim_matrix` (raw cosine matrix), and `$plot_dist` (distribution plot of
the coefficients, useful for benchmarking their empirical magnitude).

```r
cosim_results <- cosim_itens_scales(
  item_emb = item_embeddings,
  scale_emb = scale_embeddings,
  item_text = item_descriptions,
  factor_itens = item_factors,
  factor_scale = scale_factors
)

cosim_results$cosim_mat   # item x scale similarities + summary measures
cosim_results$plot_dist   # distribution of all cosine coefficients
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

This example demonstrates the complete workflow for mapping BFI-2 personality facets (Soto & John, 2017) to the 70 Facetmap scales (Irwing, Hughes, Tokarev, & Booth, 2023):


Irwing, P., Hughes, D. J., Tokarev, A., & Booth, T. (2023). Towards a taxonomy of personality facets. European Journal of Personality, 38(3), 494-515. https://doi.org/10.1177/08902070231200919 (Original work published 2024)

see https://facetmap.org

Soto, C. J., & John, O. P. (2017). Big Five Inventory-2 (BFI-2) [Database record]. APA PsycTests. https://doi.org/10.1037/t64008-000

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
scale_names <- facetmap_scale_embeddings$domain_facet

cosim_results <- cosim_itens_scales(
  item_emb = bfi_embeddings,
  scale_emb = facetmap_scale_embeddings[, -1],
  item_text = item_dic_bfi2$item_en_text,
  factor_itens = item_dic_bfi2$domain_facet,
  factor_scale = scale_names
)$cosim_mat

# Step 5: Analyze relationships
summary_results <- mean_cosim_by_item_factors(
  cosim_mat = as.matrix(cosim_results[, scale_names]),
  item_factor = cosim_results$scale,
  target_factor = scale_names
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
HF_API_KEY=your_huggingface_token_here

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

## 📄 Citation

If you use `embeddcv` in your research, please cite:

```bibtex
@software{embeddcv2025,
  title = {embeddcv: Analyzing Psychological Scales with Embeddings},
  author = {Primi, R., Franco, V. R., Mose, L. Nunes, C. H., & Cainã, A.},
  year = {2025},
  url = {https://github.com/rprimi/embeddcv}
}
```

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


## 📧 Contact

- **Authors**: R. Primi, V. R. Franco, L. Mose, C. H. Nunes, & A. Cainã 
- **Repository**: [GitHub](https://github.com/rprimi/embeddcv)
- **Issues**: [Report a bug](https://github.com/rprimi/embeddcv/issues)

---
