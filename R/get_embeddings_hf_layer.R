
# ─────────────────────────────────────────────────────────────────────────────
# Shared Python backend for layer-wise transformer embeddings.
#
# Unlike get_embeddings_hf_local() (which uses sentence-transformers and only
# exposes the final pooled output), these functions run a raw `transformers`
# forward pass with output_hidden_states=TRUE, so any hidden layer can be
# extracted and pooled. This mirrors the vsm.py / hf_represent workflow
# (Potts, CS224u) used for Divergent Semantic Integration research.
# ─────────────────────────────────────────────────────────────────────────────

# Per-session flag: has the Python helper code been sourced yet?
.embeddcv_torch_env <- new.env(parent = emptyenv())

# Python helper module (sourced once per session into the Python main namespace).
.embeddcv_py_code <- r"---(
import torch
import numpy as np
from transformers import AutoTokenizer, AutoModel

# In-memory cache of (tokenizer, model, device) keyed by model name.
_EMBEDDCV_MODELS = {}

def _embeddcv_load(model_name):
    if model_name not in _EMBEDDCV_MODELS:
        tok = AutoTokenizer.from_pretrained(model_name)
        mod = AutoModel.from_pretrained(model_name)
        mod.eval()
        device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        mod.to(device)
        _EMBEDDCV_MODELS[model_name] = (tok, mod, device)
    return _EMBEDDCV_MODELS[model_name]

def _embeddcv_hidden(text, model_name, layer, add_special_tokens):
    # Returns (hidden_states_for_text [m_tokens x n_dim] on CPU, token_strings).
    tok, mod, device = _embeddcv_load(model_name)
    enc = tok.encode(text, add_special_tokens=add_special_tokens, return_tensors="pt")
    if enc.shape[1] == 0:                       # empty after tokenisation -> UNK
        enc = torch.tensor([[tok.unk_token_id]])
    enc = enc.to(device)
    with torch.no_grad():
        out = mod(enc, output_hidden_states=True)
    # hidden_states is a tuple of length (n_layers + 1); index 0 = embeddings,
    # -1 = final layer. `layer` may be negative.
    h = out.hidden_states[int(layer)]           # (1, m, n)
    h = h.squeeze(0).detach().cpu()             # (m, n)
    toks = tok.convert_ids_to_tokens(enc.squeeze(0).cpu().tolist())
    return h, toks

def embeddcv_embed_pooled(texts, model_name, layer, pooling, add_special_tokens):
    rows = []
    for t in texts:
        h, _ = _embeddcv_hidden(t, model_name, layer, add_special_tokens)
        if pooling == "mean":
            v = torch.mean(h, dim=0)
        elif pooling == "mult_znorm":
            # Multiplicative pooling with log + z-normalisation (vsm.py).
            eps = 1e-8
            lg = torch.log(h + eps)
            mu = lg.mean(dim=0, keepdim=True)
            sd = lg.std(dim=0, keepdim=True, unbiased=False) + eps
            z  = (lg - mu) / sd
            v  = torch.exp(torch.sum(z, dim=0))
        elif pooling == "max":
            v = torch.amax(h, dim=0)
        elif pooling == "min":
            v = torch.amin(h, dim=0)
        else:
            raise ValueError("Unknown pooling: " + str(pooling))
        rows.append(v.numpy())
    return np.vstack(rows)

def embeddcv_token_dsi(texts, model_name, layer, add_special_tokens):
    results = []
    for t in texts:
        h, toks = _embeddcv_hidden(t, model_name, layer, add_special_tokens)  # (m, n)
        # Length-normalise each token vector, then cosine = normalised Gram matrix.
        norm = h / h.norm(dim=1, keepdim=True)
        cos  = torch.mm(norm, norm.t())          # (m, m), diagonal = 1
        m = cos.shape[0]
        if m > 1:
            idx  = torch.tril_indices(m, m, offset=-1)   # lower tri, no diagonal
            vals = cos[idx[0], idx[1]]
            dsi_mean = float(vals.mean().item())
            dsi_sd   = float(vals.std().item())
        else:
            dsi_mean = float("nan")
            dsi_sd   = float("nan")
        eig = torch.linalg.eigvalsh(cos)         # ascending real eigenvalues
        num_gt1 = int((eig > 1).sum().item())
        csum  = torch.cumsum(eig, dim=0)
        total = csum[-1]
        if float(total) != 0.0:
            target = 0.75 * total
            num_75 = int((csum >= target).int().argmax().item()) + 1
        else:
            num_75 = 0
        results.append({
            "tokens": list(toks),
            "cosine": cos.numpy(),
            "dsi_mean": dsi_mean,
            "dsi_sd": dsi_sd,
            "eigenvalues": eig.numpy(),
            "num_eigenvalues_gt1": num_gt1,
            "num_eigenvalues_75pct": num_75,
        })
    return results
)---"

# Ensure torch / transformers are installed and the Python helpers are sourced.
.ensure_torch_helpers <- function(python_env = NULL, install_packages = TRUE) {
  if (!is.null(python_env))
    reticulate::use_python(python_env, required = TRUE)

  for (pkg in c("torch", "transformers", "numpy")) {
    if (!reticulate::py_module_available(pkg)) {
      if (install_packages) {
        message("Installing Python package: ", pkg)
        reticulate::py_install(pkg)
      } else {
        stop("Python package '", pkg, "' is not available. ",
             "Set install_packages = TRUE to install it automatically.")
      }
    }
  }

  if (is.null(.embeddcv_torch_env$loaded)) {
    reticulate::py_run_string(.embeddcv_py_code)
    .embeddcv_torch_env$loaded <- TRUE
  }
  invisible(TRUE)
}


#' Get Sentence Embeddings from a Chosen Transformer Layer
#'
#' Generates one embedding vector per text by running a raw HuggingFace
#' \code{transformers} forward pass, extracting the hidden states of a chosen
#' layer, and pooling the token vectors. Unlike \code{get_embeddings_hf_local()}
#' (which always returns the final-layer, mask-mean, sentence-transformers
#' embedding), this lets you pick the \code{layer} and \code{pooling} strategy.
#'
#' @param text Character vector. One or more texts to embed.
#' @param model Character. A raw HuggingFace transformer model name
#'   (default \code{'xlm-roberta-base'}).
#' @param layer Integer. Which hidden-state layer to extract. \code{0} is the
#'   input embedding layer, \code{1..N} are the transformer layers, and
#'   \code{-1} (default) is the final layer. For a 12-layer BERT-style model the
#'   valid range is 0..12. Middle layers (e.g. \code{7}) often carry the richest
#'   semantic content.
#' @param pooling Character. How to aggregate token vectors into one sentence
#'   vector. One of:
#'   \describe{
#'     \item{\code{"mean"}}{Arithmetic mean over tokens (default).}
#'     \item{\code{"mult_znorm"}}{Multiplicative pooling with log + z-normalisation
#'       (sum of z-scored log token vectors, then exponentiated), as in vsm.py.}
#'     \item{\code{"max"}}{Element-wise maximum over tokens.}
#'     \item{\code{"min"}}{Element-wise minimum over tokens.}
#'   }
#' @param add_special_tokens Logical. Whether to include the model's special
#'   tokens (e.g. \code{[CLS]}/\code{<s>}) in the token set before pooling.
#'   Default \code{FALSE} (matches the vsm.py workflow).
#' @param python_env Character. Path to a Python executable or virtual
#'   environment. If \code{NULL}, uses the default reticulate environment.
#' @param install_packages Logical. Whether to auto-install \code{torch},
#'   \code{transformers}, and \code{numpy} if missing (default \code{TRUE}).
#'
#' @return A data frame with one row per input text and one column per embedding
#'   dimension (\code{V1} … \code{VN}).
#'
#' @details
#' \strong{mult_znorm caveat:} this pooling takes \code{log()} of the hidden
#' states. Transformer hidden states contain negative values, for which
#' \code{log()} is undefined (\code{NaN}). This faithfully reproduces the
#' original vsm.py behaviour; interpret the resulting vectors accordingly.
#'
#' Models are cached in-memory per R session, so repeated calls with the same
#' model do not reload it.
#'
#' @importFrom reticulate py py_run_string py_module_available py_install use_python
#'
#' @examples
#' \dontrun{
#'  texts <- c("I am outgoing", "I worry a lot")
#'
#'  # Final-layer mean pooling
#'  emb <- get_embeddings_hf_layer(texts, model = "xlm-roberta-base")
#'
#'  # Layer 7, mean pooling (as in the DSI notebook)
#'  emb7 <- get_embeddings_hf_layer(texts, model = "xlm-roberta-base",
#'                                  layer = 7, pooling = "mean")
#'
#'  # Layer 7, multiplicative-with-znorm pooling
#'  emb7m <- get_embeddings_hf_layer(texts, model = "xlm-roberta-base",
#'                                   layer = 7, pooling = "mult_znorm")
#' }
#'
#' @export
get_embeddings_hf_layer <- function(text,
                                    model              = "xlm-roberta-base",
                                    layer              = -1L,
                                    pooling            = c("mean", "mult_znorm", "max", "min"),
                                    add_special_tokens = FALSE,
                                    python_env         = NULL,
                                    install_packages   = TRUE) {

  stopifnot(is.character(text))
  pooling <- match.arg(pooling)

  .ensure_torch_helpers(python_env, install_packages)

  emb <- reticulate::py$embeddcv_embed_pooled(
    texts              = as.list(text),
    model_name         = model,
    layer              = as.integer(layer),
    pooling            = pooling,
    add_special_tokens = add_special_tokens
  )

  emb <- as.matrix(emb)
  df  <- as.data.frame(emb)
  names(df) <- paste0("V", seq_len(ncol(df)))

  message("Done: ", nrow(df), " x ", ncol(df),
          " embeddings (model = ", model, ", layer = ", layer,
          ", pooling = ", pooling, ").")

  df
}
