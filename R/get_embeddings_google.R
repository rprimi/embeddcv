#' Generate Text Embeddings using Google Gemini API
#'
#' This function generates text embeddings using Google's Gemini embedding models.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param api_key Character. Your Google API key. If not provided, will try to get from environment variable GEMINI_API_KEY.
#' @param model Character. The embedding model to use (default: 'embedding-001').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom httr POST add_headers content content_type_json
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#'  # Get embeddings for sample texts (requires API key)
#'  api_key <- "your_google_api_key_here"
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  embeddings <- get_embeddings_google(
#'    text = item_texts,
#'    api_key = api_key,
#'    model = "embedding-001"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#' }
#'
get_embeddings_google <- function(text, api_key = NULL, model = "embedding-001") {

  # Get API key from environment if not provided
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
    if (nchar(api_key) < 1) {
      stop("Google API key is required. Please provide api_key parameter or set GEMINI_API_KEY environment variable.")
    }
  }

  # Initialize results matrix
  n_texts <- length(text)
  embeddings_list <- vector("list", n_texts)

  # Process each text
  for (i in seq_along(text)) {
    # Prepare the request
    model_query <- paste0(model, ":embedContent")

    response <- POST(
      url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
      query = list(key = api_key),
      content_type_json(),
      encode = "json",
      body = list(
        model = paste0("models/", model),
        content = list(
          parts = list(
            list(text = text[i])
          )
        )
      )
    )

    # Check for successful response
    if (response$status_code == 200) {
      embeddings_list[[i]] <- unlist(content(response)$embedding$values)
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
