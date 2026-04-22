#' Generate Text Embeddings using HuggingFace Models Locally
#'
#' Generates text embeddings using HuggingFace's sentence-transformers library
#' running locally via Python (reticulate). Useful for models not available via
#' the Inference API, or for offline use.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param model Character. The sentence-transformers model to use
#'   (default: \code{'dwulff/mpnet-personality'}).
#' @param python_env Character. Path to a Python executable or virtual environment.
#'   If \code{NULL}, uses the default reticulate environment.
#' @param install_packages Logical. Whether to automatically install required Python
#'   packages if missing (default: \code{TRUE}).
#'
#' @return A data frame where each row is the embedding vector for the corresponding
#'   input text.
#'
#' @details
#' Requires the Python packages \code{sentence_transformers} and \code{torch}.
#' The model is downloaded on first use and cached locally by sentence-transformers.
#'
#' @importFrom reticulate import py_install py_module_available r_to_py py_to_r
#'
#' @examples
#' \dontrun{
#'  embeddings <- get_embeddings_hf_local(
#'    text  = c("I am outgoing", "I worry a lot", "I like art"),
#'    model = "dwulff/mpnet-personality"
#'  )
#'  dim(embeddings)
#' }
#'
#' @export
get_embeddings_hf_local <- function(text,
                                    model            = "dwulff/mpnet-personality",
                                    python_env       = NULL,
                                    install_packages = TRUE) {

  if (!is.null(python_env))
    reticulate::use_python(python_env, required = TRUE)

  required_packages <- c("sentence_transformers", "torch")
  for (pkg in required_packages) {
    if (!reticulate::py_module_available(pkg)) {
      if (install_packages) {
        message("Installing Python package: ", pkg)
        reticulate::py_install(pkg)
      } else {
        stop("Python package '", pkg, "' is not available. ",
             "Set install_packages = TRUE to install it automatically.")
      }
    }
  }

  st <- tryCatch(
    reticulate::import("sentence_transformers"),
    error = function(e) stop("Failed to import sentence_transformers: ", e$message)
  )

  message("Loading model: ", model)
  message("Note: first-time model loading may take several minutes to download.")

  model_obj <- tryCatch(
    st$SentenceTransformer(model),
    error = function(e) stop("Failed to load model '", model, "': ", e$message)
  )

  message("Generating embeddings...")
  embeddings_py <- tryCatch({
    out <- model_obj$encode(reticulate::r_to_py(text))
    out$tolist()
  }, error = function(e) stop("Failed to generate embeddings: ", e$message))

  embeddings_df <- as.data.frame(do.call(rbind, reticulate::py_to_r(embeddings_py)))

  message("Done: ", nrow(embeddings_df), " x ", ncol(embeddings_df), " embedding matrix.")
  embeddings_df
}


#' Check Local HuggingFace Python Environment
#'
#' Reports whether the required Python packages for \code{get_embeddings_hf_local()}
#' are available.
#'
#' @return A list with package availability status and Python configuration.
#'
#' @importFrom reticulate py_module_available py_config
#' @export
check_hf_models_local <- function() {

  packages_status <- list(
    sentence_transformers = reticulate::py_module_available("sentence_transformers"),
    torch                 = reticulate::py_module_available("torch")
  )

  python_info <- tryCatch(reticulate::py_config(), error = function(e) list(error = e$message))

  list(
    packages_available = packages_status,
    python_config      = python_info,
    message = if (all(unlist(packages_status)))
      "All required packages are available. You can use get_embeddings_hf_local()."
    else
      "Some packages are missing. Run get_embeddings_hf_local() with install_packages = TRUE."
  )
}
