#' Generate Text Embeddings using HuggingFace Models Locally
#'
#' This function generates text embeddings using HuggingFace's sentence-transformers library
#' running locally via Python. It allows access to models not available via the Inference API.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param model Character. The embedding model to use (default: 'dwulff/mpnet-personality').
#' @param python_env Character. Python environment to use. If NULL, uses default reticulate environment.
#' @param install_packages Logical. Whether to automatically install required Python packages (default: TRUE).
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @details
#' This function uses the reticulate package to interface with Python's sentence-transformers library.
#' It will automatically install the required packages if they're not available.
#'
#' The function downloads the model on first use, which may take some time depending on model size.
#' Subsequent calls with the same model will be faster as the model is cached locally.
#'
#' @importFrom reticulate import py_install py_module_available
#'
#' @examples
#' \dontrun{
#'  # Get embeddings for personality texts using local model
#'  item_texts <- c("I usually let others make important decisions for me.",
#'                  "I feel uncomfortable when others do not help me to make decisions.",
#'                  "People can easily make me change my mind, even when I thought I had made it up.")
#'
#'  embeddings <- get_embeddings_hf_local(
#'    text = item_texts,
#'    model = "dwulff/mpnet-personality"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#'
#'  # Try with other models
#'  embeddings_mpnet <- get_embeddings_hf_local(
#'    text = item_texts,
#'    model = "sentence-transformers/all-mpnet-base-v2"
#'  )
#' }
#'
get_embeddings_hf_local <- function(text, model = "dwulff/mpnet-personality",
                                   python_env = NULL, install_packages = TRUE) {

  # Check if reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed. Please install it with: install.packages('reticulate')")
  }

  # Set Python environment if specified
  if (!is.null(python_env)) {
    reticulate::use_python(python_env)
  }

  # Check and install required Python packages
  required_packages <- c("sentence_transformers", "torch")

  for (pkg in required_packages) {
    if (!reticulate::py_module_available(pkg)) {
      if (install_packages) {
        message(paste("Installing Python package:", pkg))
        reticulate::py_install(pkg)
      } else {
        stop(paste("Python package", pkg, "is not available. Set install_packages = TRUE to install it automatically."))
      }
    }
  }

  # Import the sentence_transformers module
  tryCatch({
    st <- reticulate::import("sentence_transformers")
  }, error = function(e) {
    stop("Failed to import sentence_transformers. Please ensure it's properly installed.")
  })

  # Load the model
  message(paste("Loading model:", model))
  message("Note: First-time model loading may take several minutes to download...")

  tryCatch({
    model_obj <- st$SentenceTransformer(model)
  }, error = function(e) {
    stop(paste("Failed to load model", model, ". Error:", e$message))
  })

  # Convert text to Python list
  text_py <- reticulate::r_to_py(text)

  # Generate embeddings
  message("Generating embeddings...")
  tryCatch({
   embeddings_py <- model_obj$encode(text_py)
   # Convert numpy array to Python list before converting to R
   embeddings_py <- embeddings_py$tolist()
  }, error = function(e) {
   stop(paste("Failed to generate embeddings. Error:", e$message))
  })

  # Convert back to R - this gives us a list of numeric vectors
  embeddings_list <- reticulate::py_to_r(embeddings_py)

  # Bind the list of vectors into a proper matrix
  embeddings_matrix <- do.call(rbind, embeddings_list)

  # Convert to data frame
  embeddings_df <- as.data.frame(embeddings_matrix)

  # Add row names for easier identification
  rownames(embeddings_df) <- text

  message(paste("Successfully generated embeddings with dimensions:",
                nrow(embeddings_df), "x", ncol(embeddings_df)))

  return(embeddings_df)
}

#' Check Available HuggingFace Models Locally
#'
#' This function checks which HuggingFace models are available locally
#' and provides information about installed models.
#'
#' @return A list with information about available models and Python environment.
#'
#' @examples
#' \dontrun{
#'  # Check what's available
#'  model_info <- check_hf_models_local()
#'  print(model_info)
#' }
#'
#' @importFrom reticulate import py_install py_module_available
#' @export
check_hf_models_local <- function() {

  # Check if reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed. Please install it with: install.packages('reticulate')")
  }

  # Check Python packages
  packages_status <- list(
    sentence_transformers = reticulate::py_module_available("sentence_transformers"),
    torch = reticulate::py_module_available("torch")
  )

  # Get Python version info
  python_info <- tryCatch({
    reticulate::py_config()
  }, error = function(e) {
    list(error = e$message)
  })

  return(list(
    packages_available = packages_status,
    python_config = python_info,
    message = if (all(unlist(packages_status))) {
      "All required packages are available. You can use get_embeddings_hf_local()"
    } else {
      "Some packages are missing. Run get_embeddings_hf_local() with install_packages = TRUE"
    }
  ))
}
