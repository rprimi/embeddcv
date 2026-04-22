#' Generate Text Embeddings using Multiple Providers
#'
#' Generates text embeddings using OpenAI, Google Gemini, HuggingFace Inference API,
#' or HuggingFace models running locally via Python.
#'
#' @param text Character vector. One or more texts to embed.
#' @param provider Character. One of \code{'openai'}, \code{'google'},
#'   \code{'huggingface'}, or \code{'huggingface_local'} (default: \code{'openai'}).
#' @param api_key Character. API key for the chosen provider. If \code{NULL}, reads
#'   from the relevant environment variable (\code{OPENAI_API_KEY}, \code{GEMINI_API_KEY},
#'   or \code{HF_API_KEY}). Not used for \code{'huggingface_local'}.
#' @param model Character. Embedding model to use. Defaults by provider:
#'   \itemize{
#'     \item \code{openai}: \code{'text-embedding-3-small'}
#'     \item \code{google}: \code{'embedding-001'}
#'     \item \code{huggingface}: \code{'BAAI/bge-large-en-v1.5'}
#'     \item \code{huggingface_local}: \code{'dwulff/mpnet-personality'}
#'   }
#' @param timeout Integer. Seconds to wait for a response (used by HuggingFace providers,
#'   default 30).
#' @param batch_size Integer. Number of texts per API request (default 10000).
#' @param python_env Character. Python executable or virtual environment path.
#'   Only used when \code{provider = 'huggingface_local'}.
#' @param install_packages Logical. Whether to auto-install missing Python packages.
#'   Only used when \code{provider = 'huggingface_local'} (default \code{TRUE}).
#'
#' @return A data frame where each row is the embedding vector for the corresponding
#'   input text.
#'
#' @examples
#' \dontrun{
#'  texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  # OpenAI
#'  get_embeddings(texts, provider = "openai", api_key = "sk-...")
#'
#'  # HuggingFace API
#'  get_embeddings(texts, provider = "huggingface", api_key = "hf_...")
#'
#'  # HuggingFace local (no API key needed)
#'  get_embeddings(texts, provider = "huggingface_local",
#'                 model = "dwulff/mpnet-personality")
#' }
#'
#' @export
get_embeddings <- function(text,
                           provider         = "openai",
                           api_key          = NULL,
                           model            = NULL,
                           timeout          = 30,
                           batch_size       = 10000L,
                           return_tokens    = FALSE,
                           python_env       = NULL,
                           install_packages = TRUE) {

  valid_providers <- c("openai", "google", "huggingface", "huggingface_local")
  if (!provider %in% valid_providers)
    stop("Invalid provider. Must be one of: ", paste(valid_providers, collapse = ", "))

  if (is.null(model)) {
    model <- switch(provider,
      "openai"           = "text-embedding-3-small",
      "google"           = "embedding-001",
      "huggingface"      = "BAAI/bge-large-en-v1.5",
      "huggingface_local"= "dwulff/mpnet-personality"
    )
  }

  switch(provider,
    "openai" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("OPENAI_API_KEY")
        if (nchar(api_key) < 1)
          stop("OpenAI API key required. Provide api_key or set OPENAI_API_KEY.")
      }
      get_embeddings_openai(text = text, api_key = api_key, model = model,
                            batch_size = batch_size)
    },
    "google" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("GEMINI_API_KEY")
        if (nchar(api_key) < 1)
          stop("Google API key required. Provide api_key or set GEMINI_API_KEY.")
      }
      get_embeddings_google(text = text, api_key = api_key, model = model)
    },
    "huggingface" = {
      if (is.null(api_key)) {
        api_key <- Sys.getenv("HF_API_KEY")
        if (nchar(api_key) < 1)
          stop("HuggingFace API token required. Provide api_key or set HF_API_KEY.")
      }
      get_embeddings_hf(text = text, api_key = api_key, model = model,
                        timeout = timeout, batch_size = batch_size,
                        return_tokens = return_tokens)
    },
    "huggingface_local" = {
      get_embeddings_hf_local(text = text, model = model,
                              python_env = python_env,
                              install_packages = install_packages)
    }
  )
}
