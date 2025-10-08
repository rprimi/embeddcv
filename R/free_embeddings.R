#' Generate Text Embeddings using HuggingFace Models
#'
#' This function generates text embeddings using HuggingFace Models.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param path Character. Path to your preferred Python environment.
#' @param model Character. The embedding model to use (Default: 'BAAI/bge-large-en-v1.5').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom reticulate conda_list import py_module_available py_install
#'
#' @examples
#' \dontrun{
#'  # Get embeddings for sample texts
#'  path <- "your_python_env_path_here"
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  embeddings <- free_embeddings(
#'    text = item_texts,
#'    path = path,
#'    model = "BAAI/bge-large-en-v1.5"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#' }
#'
free_embeddings <- function(text, path=NULL, model="BAAI/bge-large-en-v1.5") {
  # Set python environment
  if(is.null(path)) {
    Sys.setenv(RETICULATE_PYTHON = tail(conda_list()[,2], 1))
  } else {
    Sys.setenv(RETICULATE_PYTHON = path)
  }

  # Check if basic necessary packages are installed
  test <- sapply(c("transformers", "sentence_transformers"), function(g) {
    py_module_available(g)
  })
  if(!all(test)) {
    pckgs <- c("transformers==4.41.0", "sentence-transformers==4.1.0")[test]
    py_install(packages=pckgs, pip = TRUE)
  }

  # Import necessary Python library
  sentence_transformers <- import("sentence_transformers")

  # Load the wanted model (in this case, BGE-Large)
  LLM <- sentence_transformers$SentenceTransformer(model)

  # Generate embeddings
  return(LLM$encode(text))
}
