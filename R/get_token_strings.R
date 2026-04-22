#' Add Subword Token Strings to a Token Embeddings Data Frame
#'
#' Takes the output of \code{get_embeddings_hf(..., return_tokens = TRUE)} and
#' adds a \code{subword_token} column with the actual token strings by running the
#' model's tokenizer locally via Python.
#'
#' @param token_df Data frame. Output of \code{get_embeddings_hf(return_tokens = TRUE)},
#'   containing at minimum columns \code{text} (original input text) and \code{token}
#'   (integer position index).
#' @param model Character. The HuggingFace model whose tokenizer to use. Should match
#'   the model used to generate the embeddings (default: \code{'BAAI/bge-large-en-v1.5'}).
#' @param python_env Character. Path to a Python executable or virtual environment.
#'   If \code{NULL}, uses the default reticulate environment.
#' @param install_packages Logical. Whether to auto-install \code{transformers} if
#'   missing (default: \code{TRUE}).
#'
#' @return The input data frame with a new \code{subword_token} column inserted after
#'   the \code{token} column, containing the subword token string for each row
#'   (e.g. \code{"[CLS]"}, \code{"un"}, \code{"##happy"}, \code{"[SEP]"}).
#'
#' @details
#' Uses \code{transformers.AutoTokenizer} from the HuggingFace Python library. The
#' tokenizer is downloaded on first use and cached locally. Special tokens such as
#' \code{[CLS]} and \code{[SEP]} are included because the HF Inference API typically
#' returns embeddings for the full token sequence including special tokens.
#'
#' @importFrom reticulate import py_install py_module_available r_to_py py_to_r
#'
#' @examples
#' \dontrun{
#'  texts <- c("I am outgoing", "I worry a lot")
#'
#'  tok_emb <- get_embeddings_hf(texts, api_key = "hf_...", return_tokens = TRUE)
#'  tok_emb <- add_token_strings(tok_emb, model = "BAAI/bge-large-en-v1.5")
#'
#'  tok_emb[, c("text", "token", "subword_token")]
#' }
#'
#' @export
add_token_strings <- function(token_df,
                               model            = "BAAI/bge-large-en-v1.5",
                               python_env       = NULL,
                               install_packages = TRUE) {

  if (!all(c("text", "token") %in% names(token_df)))
    stop("'token_df' must have 'text' and 'token' columns (output of get_embeddings_hf with return_tokens = TRUE).")

  if (!is.null(python_env))
    reticulate::use_python(python_env, required = TRUE)

  if (!reticulate::py_module_available("transformers")) {
    if (install_packages) {
      message("Installing Python package: transformers")
      reticulate::py_install("transformers")
    } else {
      stop("Python package 'transformers' is not available. Set install_packages = TRUE.")
    }
  }

  transformers <- reticulate::import("transformers")

  message("Loading tokenizer: ", model)
  tokenizer <- tryCatch(
    transformers$AutoTokenizer$from_pretrained(model),
    error = function(e) stop("Failed to load tokenizer for '", model, "': ", e$message)
  )

  # Tokenize each unique text and build a lookup: text -> vector of token strings
  unique_texts <- unique(token_df$text)

  token_lookup <- lapply(unique_texts, function(txt) {
    ids    <- tokenizer(txt)$input_ids
    tokens <- reticulate::py_to_r(tokenizer$convert_ids_to_tokens(ids))
    data.frame(
      text          = txt,
      token         = seq_along(tokens),   # 1-based to match token_df$token
      subword_token = tokens,
      stringsAsFactors = FALSE
    )
  })

  token_lookup_df <- do.call(rbind, token_lookup)

  # Join subword_token back onto token_df by (text, token)
  result <- merge(token_df, token_lookup_df, by = c("text", "token"), all.x = TRUE)

  # Restore original row order and place subword_token right after token column
  result <- result[order(match(paste(result$text, result$token),
                               paste(token_df$text,  token_df$token))), ]

  token_pos   <- which(names(result) == "token")
  other_cols  <- setdiff(names(result), c("text", "token", "subword_token"))
  result      <- result[, c("text", "token", "subword_token", other_cols)]
  rownames(result) <- NULL

  result
}
