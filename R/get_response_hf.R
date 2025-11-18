#' Generate Responses using HuggingFace
#'
#' Generate a response given a prompt using HuggingFace API.
#'
#' @param prompt Character. The prompt to be sent to the ChatBot.
#' @param api_key Character. Your API key. If not provided, will try to get from environment variables.
#' @param model Character. The embedding model to use. Defaults to 'google/gemma-2-2b-it'.
#' @param max_tokens Integer. The maximum number of tokens for the generated response. Defaults to 128.
#' @param temperature Numeric between 0 and 1. The level of randomness of the response. Defaults to 1.
#'
#' @return A string with the ChatBot response.
#'#'
#' @importFrom httr POST add_headers timeout content
#' @importFrom jsonlite fromJSON toJSON
#'
#' @details
#' The function automatically determines the appropriate API key from the "HF_API_KEY" environment variable.
#'
#' @examples
#' \dontrun{
#'  # Get a response from Gemma (requires API key)
#'  Response <- get_response_hf("What is the most famous personality theory?")
#'  # Print the response nicely
#'  cat(Response)
#' }
#'
get_response_hf <- function(prompt = NULL, model = 'google/gemma-2-2b-it',
                            api_key = NULL, max_tokens = 128, temperature = 1) {
  # Get API key from environment if not provided
  if (is.null(api_key)) {
    api_key <- Sys.getenv("HF_API_KEY")
    if (nchar(api_key) < 1) {
      stop("HuggingFace API token is required. Please provide api_key parameter or set HF_API_KEY environment variable.")
    }
  }

  # Define the API endpoint
  url <- "https://router.huggingface.co/v1/chat/completions"

  # Set payload
  payload <- list(model = model,
                  messages = list(list(role = "user", content = prompt)),
                  max_tokens = max_tokens,
                  temperature = temperature)

  # Send request to HF
  res <- POST(url=url,
              config = add_headers(Authorization = paste("Bearer", api_key),
                                   "Content-Type" = "application/json"),
              body = toJSON(payload, auto_unbox = TRUE),
              encode = "json",
              timeout(30))

  # Return response if all is good; else return an error
  if(res$status_code == 200) {
    txt <- content(res, "text", encoding = "UTF-8")
    out <- tryCatch(fromJSON(txt), error = function(e) stop("Unkown error. Please try another model"))
    if (!is.null(out$error)) {
      stop(out$error$message)
    } else {
      out$choices$message$content
    }
  } else {
    # Error message
    cat(paste("Error status:", res$status_code,"\n"))
    response_text <- content(res, "text", encoding = "UTF-8")
    cat("Error response:\n")
    cat(response_text,"\n")
  }
}
