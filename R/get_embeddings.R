
#' Generate Text Embeddings using OpenAI API
#'
#' This function generates text embeddings using OpenAI's embedding models.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param api_key Character. Your OpenAI API key.
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param model Character. The embedding model to use (default: 'text-embedding-3-small').
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON
#' 
#' @examples
#' \dontrun{
#' # Get embeddings for sample texts (requires API key)
#' api_key <- "your_openai_api_key_here"
#' item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#' 
#' embeddings <- get_embeddings(
#'   api_key = api_key,
#'   text = item_texts,
#'   model = "text-embedding-3-small"
#' )
#' 
#' # View the structure of the embeddings
#' dim(embeddings)  # Shows number of items and embedding dimensions
#' }
#'
#' @export
get_embeddings <- function(api_key, text, model = 'text-embedding-3-small') {
  require('httr','jsonlite')
  url <- "https://api.openai.com/v1/embeddings"
  
  # Configura o cabeçalho da solicitação
  headers <- httr::add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", api_key)
  )
  
  body <- list(
    # model = "text-embedding-ada-002",
    model = model,
    input = text
  )
  
  # Faz a solicitação POST
  response <- httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))
  
  # Verifica se a solicitação foi bem-sucedida
  if (response$status_code != 200) {
    stop("Erro na solicitação: ", response$status_code, "\n", httr::content(response, "text"))
  }
  
  # Parseia a resposta JSON
  response_content <- httr::content(response, "parsed", simplifyVector = TRUE)
  
  # Transforma em dataframa
  
  response <- do.call(rbind, response_content$data$embedding)
  response <- data.frame(response)
  
  # Retorna os embeddings
  return(response)
}
