#' Generate Text Embeddings using HuggingFace Inference API
#'
#' Generates text embeddings using HuggingFace's Inference API. By default returns
#' one mean-pooled embedding per text. Set \code{return_tokens = TRUE} to get
#' token-level embeddings instead.
#'
#' @param text Character vector. One or more texts to be transformed into embeddings.
#' @param api_key Character. Your HuggingFace API token. If not provided, will try
#'   to get from environment variable \code{HF_API_KEY}.
#' @param model Character. The embedding model to use (default: \code{'BAAI/bge-large-en-v1.5'}).
#' @param timeout Integer. Seconds to wait for a response (default 30).
#' @param batch_size Integer. Number of texts per request (default 100).
#' @param return_tokens Logical. If \code{FALSE} (default), returns one mean-pooled
#'   embedding row per input text. If \code{TRUE}, returns one row per token per text,
#'   with columns \code{text}, \code{token} (integer index — the API does not return
#'   actual token strings), and one column per embedding dimension. Only applicable
#'   when the model returns token-level embeddings; sentence-level models are unaffected.
#'
#' @return
#'   \describe{
#'     \item{return_tokens = FALSE}{A data frame \[n_texts x n_dims\] of mean-pooled embeddings.}
#'     \item{return_tokens = TRUE}{A data frame with columns \code{text}, \code{token}
#'       (integer position), and \code{dim_1} … \code{dim_N}.}
#'   }
#'
#' @importFrom httr POST add_headers timeout content
#' @importFrom jsonlite fromJSON toJSON
#'
#' @examples
#' \dontrun{
#'  texts <- c("I am outgoing", "I worry a lot", "I like art")
#'
#'  # Sentence embeddings (default)
#'  emb <- get_embeddings_hf(texts, api_key = "hf_...")
#'  dim(emb)
#'
#'  # Token-level embeddings
#'  tok <- get_embeddings_hf(texts, api_key = "hf_...", return_tokens = TRUE)
#'  head(tok[, 1:5])
#' }
#'
#' @export
get_embeddings_hf <- function(text,
                               api_key       = NULL,
                               model         = "BAAI/bge-large-en-v1.5",
                               timeout       = 30,
                               batch_size    = 100L,
                               return_tokens = FALSE) {

  if (is.null(api_key)) {
    api_key <- Sys.getenv("HF_API_KEY")
    if (nchar(api_key) < 1)
      stop("HuggingFace API token required. Provide api_key or set HF_API_KEY environment variable.")
  }

  api_url <- paste0("https://router.huggingface.co/hf-inference/models/", model)
  batches <- split(text, ceiling(seq_along(text) / batch_size))
  all_results <- vector("list", length(batches))

  for (i in seq_along(batches)) {
    batch <- batches[[i]]

    response <- POST(
      url    = api_url,
      config = add_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body   = toJSON(list(inputs = batch, options = list(wait_for_model = TRUE)),
                      auto_unbox = TRUE),
      encode = "raw",
      timeout(timeout)
    )

    if (response$status_code != 200) {
      cat("Error status:", response$status_code, "\n")
      cat(content(response, "text", encoding = "UTF-8"), "\n")
      stop("HuggingFace API request failed for batch ", i)
    }

    parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))

    # Normalise to a list-of-items, one entry per text in the batch
    # parsed can be: list of vectors, list of matrices, or a bare matrix (single text)
    items <- if (is.list(parsed)) {
      parsed
    } else if (is.matrix(parsed)) {
      # single text returned as a plain matrix (tokens x dims)
      list(parsed)
    } else {
      list(as.numeric(parsed))
    }

    if (return_tokens) {
      # ── Token-level: one row per (text, token) ────────────────────────────────
      batch_rows <- lapply(seq_along(items), function(j) {
        x <- items[[j]]

        # x is a matrix [tokens x dims] or a vector [dims] (sentence-level model)
        if (is.matrix(x)) {
          token_mat <- x
        } else {
          token_mat <- matrix(as.numeric(x), nrow = 1)
        }

        n_tokens <- nrow(token_mat)
        df <- as.data.frame(token_mat)
        colnames(df) <- paste0("dim_", seq_len(ncol(df)))
        df$text  <- batch[[j]]
        df$token <- seq_len(n_tokens)
        df[, c("text", "token", setdiff(names(df), c("text", "token")))]
      })

      all_results[[i]] <- do.call(rbind, batch_rows)

    } else {
      # ── Sentence-level: mean-pool tokens → one row per text ───────────────────
      batch_mat <- do.call(rbind, lapply(items, function(x) {
        if (is.matrix(x)) colMeans(x)
        else as.numeric(x)
      }))

      all_results[[i]] <- as.data.frame(batch_mat)
    }
  }

  do.call(rbind, all_results)
}
