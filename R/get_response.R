#' Generate Responses using Multiple Providers
#'
#' Generate a response given a prompt using OpenAI, Google Gemini, or HuggingFace APIs.
#'
#' @param prompt Character. The prompt to be sent to the ChatBot.
#' @param provider Character. The embedding provider to use: 'openai', 'google', or 'huggingface' (default: 'huggingface').
#' @param api_key Character. Your API key for the chosen provider. If not provided, will try to get from environment variables.
#' @param model Character. The embedding model to use. Defaults vary by provider:
#'   - OpenAI: 'text-embedding-3-small'
#'   - Google: 'embedding-001'
#'   - HuggingFace: 'google/gemma-2-2b-it'
#' @param max_tokens Integer. The maximum number of tokens for the generated response. Defaults to 128.
#' @param temperature Numeric between 0 and 1. The level of randomness of the response. Defaults to 1.
#'
#' @return A string with the ChatBot response.
#'
#' @details
#' The function automatically determines the appropriate API key environment variable based on the provider:
#' - OpenAI: OPENAI_API_KEY
#' - Google: GEMINI_API_KEY
#' - HuggingFace: HF_API_KEY
#'
#' @examples
#' \dontrun{
#'  # Get a response from Gemma (requires API key)
#'  Response <- get_response("What is the most famous personality theory?")
#'  # Print the response nicely
#'  cat(Response)
#' }
#'
#' @export
get_response <- function(prompt, provider = "huggingface", api_key = NULL,
                         model = NULL, max_tokens = 128, temperature = 1) {
  # Validate prompt
  if(length(prompt) > 1) stop("Argument 'prompt' should be a single string.")

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
                   "huggingface" = "google/gemma-2-2b-it")
  }

  # Validate max tokens
  if(max_tokens < 1) {
    stop("Argument 'max_tokens' should be at least 1.")
  }

  # Validate temperature
  if({temperature > 1} | {temperature < 0}) {
    stop("Argument 'temperature' should be between 0 and 1.")
  }

  # Route to appropriate function based on provider
  switch(provider,
         "openai" = {
           stop("Not implemented yet! Please use huggingface instead.")
         },
         "google" = {
           stop("Not implemented yet! Please use huggingface instead.")
         },
         "huggingface" = {
           get_response_hf(prompt=prompt, model = model,
                           max_tokens = max_tokens,
                           temperature = temperature,
                           api_key = api_key)
         })
}
