#' Generate Text Embeddings using HuggingFace API
#'
#' This function generates text embeddings using HuggingFace's Inference API.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param api_key Character. Your HuggingFace API token. If not provided, will try to get from environment variable HF_API_KEY.
#' @param model Character. The embedding model to use (default: 'BAAI/bge-large-en-v1.5').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom httr POST add_headers timeout content
#' @importFrom jsonlite fromJSON toJSON
#'
#' @examples
#' \dontrun{
#'  # Get embeddings for sample texts (requires API key)
#'  api_key <- "your_huggingface_api_token_here"
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  embeddings <- get_embeddings_hf(
#'    text = item_texts,
#'    api_key = api_key,
#'    model = "BAAI/bge-large-en-v1.5"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#' }
#'
get_embeddings_hf <- function(text, api_key = NULL, model = "BAAI/bge-large-en-v1.5") {

  # Get API key from environment if not provided
  if (is.null(api_key)) {
    api_key <- Sys.getenv("HF_API_KEY")
    if (nchar(api_key) < 1) {
      stop("HuggingFace API token is required. Please provide api_key parameter or set HF_API_KEY environment variable.")
    }
  }

  # Define the API endpoint
  api_url <- paste0("https://router.huggingface.co/hf-inference/models/", model)

  # Set request body
  request_body <- list(inputs = text,  # Just pass the texts directly
                       options = list(wait_for_model = TRUE))

  # Send request to HF
  response <- POST(url = api_url,
                   config = add_headers(Authorization = paste("Bearer", api_key),
                                        "Content-Type" = "application/json"),
                   body = toJSON(request_body, auto_unbox = TRUE),
                   encode = "json",
                   timeout(30))

  # Return embeddings if all is good; else return an error
  if(response$status_code == 200) {
    # Get the raw content as text first
    response_text <- content(response, "text", encoding = "UTF-8")
    # Parse the JSON manually
    embeddings_df <- fromJSON(response_text)
    # Return embedding
    return(embeddings_df)
  } else {
    # Error message
    cat(paste("Error status:", response$status_code,"\n"))
    response_text <- content(response, "text", encoding = "UTF-8")
    cat("Error response:\n")
    cat(response_text,"\n")
  }
}
