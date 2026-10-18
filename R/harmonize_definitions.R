#' Default template for scale definitions
#'
#' A template for definitions of psychological (trait) scales, following
#' common recommendations for construct definitions (Podsakoff, MacKenzie &
#' Podsakoff, 2016; Clark & Watson, 2019): name the core attribute, describe
#' its manifestations concretely, give both poles, and keep causes, outcomes
#' and correlates out of the definition. The high/low description stays close
#' to how items are written (what people typically think, feel and do).
#'
#' @return A character string, the template used by [harmonize_definitions()]
#'   and, optionally, by [empirical_relabeling()].
#'
#' @references
#' Podsakoff, P. M., MacKenzie, S. B., & Podsakoff, N. P. (2016).
#' Recommendations for creating better concept definitions in the
#' organizational, behavioral, and social sciences. Organizational Research
#' Methods, 19(2), 159-203. https://doi.org/10.1177/1094428115624965
#'
#' Clark, L. A., & Watson, D. (2019). Constructing validity: New developments
#' in creating objective measuring instruments. Psychological Assessment,
#' 31(12), 1412-1427. https://doi.org/10.1037/pas0000626
#'
#' @examples
#' cat(definition_template())
#' @export
definition_template <- function() {
  paste(
    "Individual differences in <core attribute, named by its high pole>.",
    "People high on it <two or three characteristic thoughts, feelings or behaviours,",
    "stated concretely as they show up in everyday situations>;",
    "people low on it <the opposite pattern>.",
    "",
    "- One or two sentences, 30 to 60 words.",
    "- Describe a stable tendency (how people usually are), not a single act or a state.",
    "- Concrete, observable manifestations, in plain language, close to how questionnaire items are phrased.",
    "- No causes, consequences, correlates or examples of measures.",
    "- Do not name the scale, the test or the label.",
    sep = "\n"
  )
}

#' Harmonize scale definitions with a template
#'
#' Asks an LLM to rewrite each definition so that it follows a common template
#' (by default [definition_template()]), keeping its meaning. Definitions that
#' already follow the template are returned with minimal changes.
#'
#' Definitions written by different authors differ in style (e.g. all FacetMap
#' definitions start with "Propensity to ..."), and embedding models pick this
#' up: definitions of the same test look alike even when their content
#' differs. Rewriting all definitions in one format removes most of that style
#' difference before the definitions are embedded.
#'
#' @details
#' Only the form is harmonized: the LLM sees the definition (and, if given, the
#' scale label), not the items. Rewriting definitions from the items would turn
#' them into item summaries and make a comparison of items and definitions
#' (jingle-jangle analysis) circular.
#'
#' @param definitions Character vector of definitions. Names, if any, are kept.
#' @param template Template text. Defaults to [definition_template()].
#' @param labels Optional character vector of scale labels (same length), shown
#'   to the LLM as context to interpret short definitions.
#' @param provider,model,chat_args LLM settings, as in [llm_label_items()].
#' @param cache_dir Optional folder for caching each answer.
#' @param max_retries Number of attempts per definition.
#' @param verbose Print progress.
#'
#' @return Character vector of harmonized definitions, same length, order and
#'   names as `definitions`.
#'
#' @examples
#' \dontrun{
#' harmonize_definitions(
#'   c(aes = "Artistic interests and appreciation of beauty",
#'     imag = "Propensity to fantasize and daydream"),
#'   labels = c("Aesthetic Sensitivity", "Imagination"),
#'   provider = "openai", model = "gpt-4.1-mini"
#' )
#' }
#' @export
harmonize_definitions <- function(
    definitions,
    template = definition_template(),
    labels = NULL,
    provider = c("openai", "anthropic", "google", "deepseek"),
    model = "gpt-4.1-mini",
    chat_args = list(),
    cache_dir = NULL,
    max_retries = 3,
    verbose = TRUE) {

  provider <- match.arg(provider)
  if (!is.null(labels) && length(labels) != length(definitions)) {
    stop("`labels` must have the same length as `definitions`.", call. = FALSE)
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package `ellmer` is required. Install it with install.packages(\"ellmer\").",
         call. = FALSE)
  }
  type <- ellmer::type_object(
    definition = ellmer::type_string("The harmonized definition.")
  )

  out <- character(length(definitions))
  for (i in seq_along(definitions)) {
    prompt <- paste0(
      "Rewrite the definition of a psychological scale so that it follows the ",
      "template below. If it already follows the template, return it with only ",
      "minimal changes.\n\n",
      "Template:\n", template, "\n\n",
      "Rules:\n",
      "- Keep the meaning and scope of the original definition: do not add ",
      "content, facets or behaviours that it does not imply, and do not drop ",
      "any of its content.\n",
      "- If the original names only one pole, infer the other pole from it.\n\n",
      if (!is.null(labels)) paste0("Scale label: ", labels[[i]], "\n"),
      "Original definition: ", definitions[[i]]
    )
    res <- embeddcv_llm_structured(prompt, type, provider, model, chat_args,
                                   cache_dir, max_retries)
    out[i] <- as.character(res$definition)
    if (verbose && (i %% 20 == 0 || i == length(definitions))) {
      message("Harmonized ", i, " of ", length(definitions), " definitions.")
    }
  }
  names(out) <- names(definitions)
  out
}
