#' Generate Text Embeddings using HuggingFace Models Locally
#'
#' Generates text embeddings using HuggingFace's sentence-transformers library
#' running locally via Python (reticulate). Useful for models not available via
#' the Inference API, or for offline use.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param model Character. The sentence-transformers model to use
#'   (default: \code{'dwulff/mpnet-personality'}).
#' @param batch_size Integer. Number of texts per encode call (default 10000).
#'   Lower this if you hit memory issues with large inputs.
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
#' Loaded models are also cached in-memory for the duration of the R session, so
#' repeated calls with the same model do not reload it.
#'
#' @importFrom reticulate import py_install py_module_available py_to_r use_python
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
                                    batch_size       = 10000L,
                                    python_env       = NULL,
                                    install_packages = TRUE) {

  stopifnot(is.character(text))

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

  # Keep Python objects un-auto-converted so we can call methods like .tolist()
  # if needed, and use py_to_r() explicitly.
  st <- tryCatch(
    reticulate::import("sentence_transformers", convert = FALSE),
    error = function(e) stop("Failed to import sentence_transformers: ", e$message)
  )

  # In-memory model cache (per R session) — avoids reloading on each call.
  model_key <- make.names(model)
  if (exists(model_key, envir = .hf_local_model_cache, inherits = FALSE)) {
    model_obj <- get(model_key, envir = .hf_local_model_cache, inherits = FALSE)
  } else {
    message("Loading model: ", model)
    message("Note: first-time model loading may take several minutes to download.")
    model_obj <- tryCatch(
      st$SentenceTransformer(model),
      error = function(e) stop("Failed to load model '", model, "': ", e$message)
    )
    assign(model_key, model_obj, envir = .hf_local_model_cache)
  }

  batch_size <- as.integer(batch_size)
  if (batch_size <= 0) stop("batch_size must be a positive integer.")

  idx_batches <- split(seq_along(text), ceiling(seq_along(text) / batch_size))
  res_list    <- vector("list", length(idx_batches))

  for (b in seq_along(idx_batches)) {
    idx        <- idx_batches[[b]]
    text_batch <- text[idx]

    message(sprintf("Generating local HF embeddings batch %d/%d (%d texts)",
                    b, length(idx_batches), length(text_batch)))

    embeddings_raw <- tryCatch(
      model_obj$encode(
        as.list(text_batch),
        convert_to_numpy  = TRUE,
        show_progress_bar = FALSE
      ),
      error = function(e) stop("Failed to generate embeddings: ", e$message)
    )

    embeddings <- reticulate::py_to_r(embeddings_raw)

    # py_to_r can return either a matrix (numpy 2D) or a list (numpy 1D per text)
    if (is.list(embeddings) && is.null(dim(embeddings)))
      embeddings <- do.call(rbind, embeddings)

    embeddings <- as.matrix(embeddings)

    # Single-text edge case: ensure result is [1 x dims], not [dims x 1]
    if (length(text_batch) == 1L && nrow(embeddings) != 1L)
      embeddings <- matrix(embeddings, nrow = 1)

    res_list[[b]] <- embeddings
  }

  embeddings_matrix <- do.call(rbind, res_list)
  embeddings_df     <- as.data.frame(embeddings_matrix)
  names(embeddings_df) <- paste0("V", seq_len(ncol(embeddings_df)))

  message("Done: ", nrow(embeddings_df), " x ",
          ncol(embeddings_df), " embedding matrix.")

  embeddings_df
}


# Per-session cache of loaded SentenceTransformer models.
# Initialised at package load time.
.hf_local_model_cache <- new.env(parent = emptyenv())


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
