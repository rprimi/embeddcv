
#' Token-Level Cosine Similarity and Divergent Semantic Integration (DSI)
#'
#' For each input text, extracts the token embedding matrix from a chosen
#' transformer layer, computes the cosine similarity matrix among its tokens,
#' and derives Divergent Semantic Integration (DSI) metrics from that matrix.
#' This reproduces the \code{vsm.divergent_semantic_integration} workflow
#' (Johnson et al., 2021) used in creativity research.
#'
#' @param text Character vector. One or more texts to analyse.
#' @param model Character. A raw HuggingFace transformer model name
#'   (default \code{'xlm-roberta-base'}).
#' @param layer Integer. Which hidden-state layer to use (default \code{-1},
#'   the final layer). \code{0} is the input embedding layer. See
#'   \code{\link{get_embeddings_hf_layer}} for details.
#' @param add_special_tokens Logical. Whether to include the model's special
#'   tokens (e.g. \code{[CLS]}/\code{<s>}) among the tokens (default \code{FALSE}).
#' @param python_env Character. Path to a Python executable or virtual
#'   environment. If \code{NULL}, uses the default reticulate environment.
#' @param install_packages Logical. Whether to auto-install \code{torch},
#'   \code{transformers}, and \code{numpy} if missing (default \code{TRUE}).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{summary}{A data frame with one row per input text: \code{text},
#'       \code{n_tokens}, \code{dsi_mean}, \code{dsi_sd},
#'       \code{num_eigenvalues_gt1}, and \code{num_eigenvalues_75pct}.}
#'     \item{details}{A named list (one element per text) where each element
#'       contains: \code{tokens} (character vector of subword tokens),
#'       \code{cosine} (token x token cosine similarity matrix, with token
#'       strings as row/column names), \code{eigenvalues} (numeric vector of
#'       the cosine matrix eigenvalues, ascending), and the per-text DSI scalars
#'       (\code{dsi_mean}, \code{dsi_sd}, \code{num_eigenvalues_gt1},
#'       \code{num_eigenvalues_75pct}).}
#'   }
#'
#' @details
#' For each text the token vectors (from \code{layer}) are length-normalised and
#' multiplied to form a token x token cosine similarity matrix (diagonal = 1).
#' \itemize{
#'   \item \code{dsi_mean} / \code{dsi_sd}: mean and SD of the strictly lower
#'     triangle (pairwise token cosines, diagonal excluded). Lower mean cosine =
#'     more semantically divergent text.
#'   \item \code{eigenvalues}: eigenvalues of the full cosine matrix.
#'   \item \code{num_eigenvalues_gt1}: count of eigenvalues > 1.
#'   \item \code{num_eigenvalues_75pct}: number of (ascending-cumulative)
#'     eigenvalues needed to reach 75\% of their total, reproducing the original
#'     vsm.py computation.
#' }
#' Texts with a single token yield \code{NaN} for \code{dsi_mean}/\code{dsi_sd}
#' (no token pairs).
#'
#' @seealso \code{\link{get_embeddings_hf_layer}} for pooled sentence embeddings.
#'
#' @importFrom reticulate py
#'
#' @examples
#' \dontrun{
#'  res <- token_dsi(c("The angry sea swallowed the boat",
#'                     "He bought milk and bread"),
#'                   model = "xlm-roberta-base", layer = 7)
#'
#'  res$summary                       # DSI metrics per text
#'  res$details[[1]]$cosine           # token x token cosine matrix
#'  res$details[[1]]$eigenvalues      # eigenvalues of that matrix
#' }
#'
#' @export
token_dsi <- function(text,
                      model              = "xlm-roberta-base",
                      layer              = -1L,
                      add_special_tokens = FALSE,
                      python_env         = NULL,
                      install_packages   = TRUE) {

  stopifnot(is.character(text))

  .ensure_torch_helpers(python_env, install_packages)

  res <- reticulate::py$embeddcv_token_dsi(
    texts              = as.list(text),
    model_name         = model,
    layer              = as.integer(layer),
    add_special_tokens = add_special_tokens
  )

  # Per-text summary data frame
  summary_df <- do.call(rbind, lapply(seq_along(res), function(i) {
    r <- res[[i]]
    data.frame(
      text                  = text[i],
      n_tokens              = length(unlist(r$tokens)),
      dsi_mean              = r$dsi_mean,
      dsi_sd                = r$dsi_sd,
      num_eigenvalues_gt1   = r$num_eigenvalues_gt1,
      num_eigenvalues_75pct = r$num_eigenvalues_75pct,
      stringsAsFactors      = FALSE
    )
  }))

  # Detailed per-text objects (named, labelled matrices)
  details <- lapply(res, function(r) {
    toks <- unlist(r$tokens)
    cs   <- r$cosine
    if (is.matrix(cs)) {
      rownames(cs) <- toks
      colnames(cs) <- toks
    }
    list(
      tokens                = toks,
      cosine                = cs,
      eigenvalues           = as.numeric(r$eigenvalues),
      dsi_mean              = r$dsi_mean,
      dsi_sd                = r$dsi_sd,
      num_eigenvalues_gt1   = r$num_eigenvalues_gt1,
      num_eigenvalues_75pct = r$num_eigenvalues_75pct
    )
  })
  names(details) <- text

  list(summary = summary_df, details = details)
}
