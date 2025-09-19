#' Generate Text Embeddings using OpenAI API or HuggingFace Models
#'
#' This function generates text embeddings using OpenAI's embedding or HuggingFace
#' models. It accepts vectors of text and returns embeddings as a data frame. If
#' using HuggingFace models, please be aware that you have to download those models
#' to your own machine. Therefore, always be sure that you have enough memory space
#' before downloading any model and that it comes from a source you trust
#' (more information at: https://huggingface.co/models?pipeline_tag=feature-extraction).
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param path Character. Either your OpenAI API key (if using a OpenAI API model),
#'             or the path to your preferred Python environment.
#' @param model Character. The embedding model to use (default: 'text-embedding-3-small').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @examples
#'  \dontrun{
#'  # Get embeddings for sample texts (requires API key)
#'  api_key <- "your_openai_api_key_here"
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  embeddings <- get_embeddings(
#'    text = item_texts,
#'    path = api_key,
#'    model = "text-embedding-3-small"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#'  }
#'
#' @export
get_embeddings <- function(text, path=NULL, model='text-embedding-3-small') {
  if(length(grep("/", model)) == 0) {
    openai_embeddings(text=text, api_key=path, model=model)
  } else {
    free_embeddings(text=text, path=path, model=model)
  }
}
