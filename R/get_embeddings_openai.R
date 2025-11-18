#' Generate Text Embeddings using OpenAI API
#'
#' This function generates text embeddings using OpenAI's embedding models.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param api_key Character. Your OpenAI API key.
#' @param model Character. The embedding model to use (Default: 'text-embedding-3-small').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#'  # Get embeddings for sample texts (requires API key)
#'  api_key <- "your_openai_api_key_here"
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  embeddings <- get_embeddings_openai(
#'    text = item_texts,
#'    api_key = api_key,
#'    model = "text-embedding-3-small"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings)  # Shows number of items and embedding dimensions
#' }
#'
get_embeddings_openai <- function(text, api_key, model = 'text-embedding-3-small') {
  # Online path to OpenAI's API
  url <- "https://api.openai.com/v1/embeddings"

  # Configura o cabeçalho da solicitação
  headers <- add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", api_key)
  )

  body <- list(
    model = model,
    input = text
  )

  # Faz a solicitação POST
  response <- POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))

  # Verifica se a solicitação foi bem-sucedida
  if (response$status_code != 200) {
    stop("Request error: ", response$status_code, "\n", content(response, "text"))
  }

  # Parseia a resposta JSON
  response_content <- content(response, "parsed", simplifyVector = TRUE)

  # Transforma em dataframa
  response <- do.call(rbind, response_content$data$embedding)
  response <- data.frame(response)

  # Retorna os embeddings
  return(response)
}
