#' Generate Text Embeddings using HuggingFace API
#'
#' This function generates text embeddings using HuggingFace's Inference API.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param api_key Character. Your HuggingFace API token. If not provided, will try to get from environment variable HF_API_KEY.
#' @param model Character. The embedding model to use (default: 'sentence-transformers/all-MiniLM-L6-v2').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON
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
#'    model = "sentence-transformers/all-MiniLM-L6-v2"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#' }
#'
#' @export
get_embeddings_hf <- function(text, api_key = NULL, model = "sentence-transformers/all-MiniLM-L6-v2") {
  
  # Get API key from environment if not provided
  if (is.null(api_key)) {
    api_key <- Sys.getenv("HF_API_KEY")
    if (nchar(api_key) < 1) {
      stop("HuggingFace API token is required. Please provide api_key parameter or set HF_API_KEY environment variable.")
    }
  }
  
  # Define the API endpoint
  api_url <- paste0("https://api-inference.huggingface.co/pipeline/feature-extraction/", model)
  
  # Initialize results matrix
  n_texts <- length(text)
  embeddings_list <- vector("list", n_texts)
  
  # Process each text
  for (i in seq_along(text)) {
    # Prepare the request body
    request_body <- list(inputs = text[i])
    
    # Make the POST request
    response <- POST(
      url = api_url,
      add_headers(Authorization = paste("Bearer", api_key)),
      body = toJSON(request_body, auto_unbox = TRUE),
      encode = "json"
    )
    
    # Check for successful response
    if (response$status_code == 200) {
      response_content <- content(response, "parsed")
      # HuggingFace returns embeddings in a nested structure
      if (is.list(response_content) && length(response_content) > 0) {
        embeddings_list[[i]] <- response_content[[1]]$embedding
      } else {
        embeddings_list[[i]] <- response_content
      }
    } else {
      stop("API request failed for text ", i, " with status: ", response$status_code, 
           "\nResponse: ", content(response, "text"))
    }
  }
  
  # Convert list to matrix/data frame
  embeddings_matrix <- do.call(rbind, embeddings_list)
  embeddings_df <- data.frame(embeddings_matrix)
  
  return(embeddings_df)
}