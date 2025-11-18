#' Generate Text Embeddings using Multiple Providers
#'
#' This function generates text embeddings using OpenAI, Google Gemini, or HuggingFace APIs.
#' It accepts vectors of text and returns embeddings as a data frame.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param provider Character. The embedding provider to use: 'openai', 'google', or 'huggingface' (default: 'openai').
#' @param api_key Character. Your API key for the chosen provider. If not provided, will try to get from environment variables.
#' @param model Character. The embedding model to use. Defaults vary by provider:
#'   - OpenAI: 'text-embedding-3-small'
#'   - Google: 'embedding-001'
#'   - HuggingFace: 'BAAI/bge-large-en-v1.5'
#'
#' @return A data frame where each row represents an embedding vector for the corresponding input text.
#'
#' @details
#' The function automatically determines the appropriate API key environment variable based on the provider:
#' - OpenAI: OPENAI_API_KEY
#' - Google: GEMINI_API_KEY
#' - HuggingFace: HF_API_KEY
#'
#' @examples
#' \dontrun{
#'  # Get embeddings using OpenAI (requires API key)
#'  item_texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  # OpenAI embeddings
#'  embeddings_openai <- get_embeddings(
#'    text = item_texts,
#'    provider = "openai",
#'    api_key = "your_openai_api_key",
#'    model = "text-embedding-3-small"
#'  )
#'
#'  # Google embeddings
#'  embeddings_google <- get_embeddings(
#'    text = item_texts,
#'    provider = "google",
#'    api_key = "your_google_api_key",
#'    model = "embedding-001"
#'  )
#'
#'  # HuggingFace embeddings
#'  embeddings_hf <- get_embeddings(
#'    text = item_texts,
#'    provider = "huggingface",
#'    api_key = "your_hf_api_token",
#'    model = "BAAI/bge-large-en-v1.5"
#'  )
#'
#'  # View the structure of the embeddings
#'  dim(embeddings_openai)  # Shows number of items and embedding dimensions
#' }
#'
#' @export
get_embeddings <- function(text, provider = "openai", api_key = NULL, model = NULL) {

  # Validate provider
  valid_providers <- c("openai", "google", "huggingface")
  if (!provider %in% valid_providers) {
    stop("Invalid provider. Must be one of: ", paste(valid_providers, collapse = ", "))
  }

  # Set default models based on provider
  if (is.null(model)) {
    model <- switch(provider,
                   "openai" = "text-embedding-3-small",
                   "google" = "embedding-001",
                   "huggingface" = "BAAI/bge-large-en-v1.5")
  }

  # Route to appropriate function based on provider
  switch(provider,
         "openai" = {
           if (is.null(api_key)) {
             api_key <- Sys.getenv("OPENAI_API_KEY")
             if (nchar(api_key) < 1) {
               stop("OpenAI API key is required. Please provide api_key parameter or set OPENAI_API_KEY environment variable.")
             }
           }
           get_embeddings_openai(text = text, api_key = api_key, model = model)
         },
         "google" = {
           get_embeddings_google(text = text, api_key = api_key, model = model)
         },
         "huggingface" = {
           get_embeddings_hf(text = text, api_key = api_key, model = model)
         })
}
