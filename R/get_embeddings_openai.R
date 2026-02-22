#' Generate Text Embeddings using OpenAI API (batched)
#'
#' This function generates text embeddings using OpenAI's embedding models.
#' It accepts vectors of text and returns embeddings as a data frame.
#' For large inputs, it automatically batches requests to avoid token limits.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param api_key Character. Your OpenAI API key.
#' @param model Character. The embedding model to use (Default: 'text-embedding-3-small').
#' @param batch_size Integer. Number of texts per request (Default: 10000).
#' @param sleep Numeric. Seconds to pause between batches (Default: 0). Useful for rate limits.
#' @param progress Logical. Print simple progress messages (Default: TRUE).
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @importFrom httr POST add_headers content status_code timeout
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#'  api_key <- "your_openai_api_key_here"
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  embeddings <- get_embeddings_openai(
#'    text = item_texts,
#'    api_key = api_key,
#'    model = "text-embedding-3-small",
#'    batch_size = 200
#'  )
#'  dim(embeddings)
#' }
get_embeddings_openai <- function(text,
                                  api_key,
                                  model = "text-embedding-3-small",
                                  batch_size = 10000L,
                                  sleep = 0,
                                  progress = TRUE) {
 stopifnot(is.character(text))
 batch_size <- as.integer(batch_size)
 if (batch_size <= 0) stop("batch_size must be a positive integer.")
 
 url <- "https://api.openai.com/v1/embeddings"
 
 headers <- httr::add_headers(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", api_key)
 )
 
 # inner function: one API call for one batch
 call_one_batch <- function(text_batch) {
  body <- list(model = model, input = text_batch)
  
  # Prefer encode="json" for correctness/stability
  resp <- httr::POST(
   url,
   headers,
   body = body,
   encode = "json",
   httr::timeout(120)
  )
  
  if (httr::status_code(resp) != 200) {
   stop(
    "Request error: ", httr::status_code(resp), "\n",
    httr::content(resp, "text", encoding = "UTF-8")
   )
  }
  
  out <- httr::content(resp, "parsed", simplifyVector = TRUE)
  emb <- do.call(rbind, out$data$embedding)
  as.data.frame(emb)
 }
 
 n <- length(text)
 if (n == 0) return(data.frame())
 
 # If small enough, do a single call
 if (n <= batch_size) return(call_one_batch(text))
 
 # Otherwise, batch
 idx_batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
 nb <- length(idx_batches)
 
 res_list <- vector("list", nb)
 
 for (b in seq_len(nb)) {
  idx <- idx_batches[[b]]
  if (progress) message(sprintf("Embeddings batch %d/%d (%d texts)", b, nb, length(idx)))
  
  res_list[[b]] <- call_one_batch(text[idx])
  
  if (sleep > 0 && b < nb) Sys.sleep(sleep)
 }
 
 # Preserve original order by construction
 out <- do.call(rbind, res_list)
 rownames(out) <- NULL
 out
}