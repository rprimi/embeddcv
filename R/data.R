#' BFI-2 Items Dictionary
#'
#' A dataset containing the Big Five Inventory-2 (BFI-2) items in Portuguese and English,
#' along with their domain and facet classifications.
#'
#' @format A data frame with 63 rows and 8 variables:
#' \describe{
#'   \item{coditem}{Character. Item code (e.g., "bfi2_5")}
#'   \item{item_pt_text}{Character. Item text in Portuguese}
#'   \item{item_en_text}{Character. Item text in English}
#'   \item{domain}{Factor. Big Five domain (O, C, E, A, N)}
#'   \item{facet}{Character. Facet within domain (e.g., "aes" for aesthetics)}
#'   \item{pole}{Numeric. Item polarity (0 = negative, 1 = positive)}
#'   \item{seman_pairs}{Numeric. Semantic pair grouping}
#'   \item{domain_facet}{Character. Combined domain and facet label (e.g., "O_aes")}
#' }
#' @source Big Five Inventory-2 (BFI-2) psychological assessment
NULL

#' Facetmap Items Dictionary
#'
#' A dataset containing items from various psychological facets mapped to Big Five domains,
#' including absorption, creativity, and other personality facets.
#'
#' @format A data frame with 389 rows and 8 variables:
#' \describe{
#'   \item{item_number}{Integer. Sequential item number}
#'   \item{item_text}{Character. Full text of the item}
#'   \item{reverse_code}{Integer. Reverse coding indicator (1 = normal, other = reverse)}
#'   \item{num}{Numeric. Numeric scale identifier}
#'   \item{domain_facet}{Character. Domain and facet combination (e.g., "O_Absorption")}
#'   \item{key_facet}{Character. Key facet name (e.g., "absorption")}
#'   \item{Prim}{Character. Primary Big Five domain (O, C, E, A, N)}
#'   \item{Sec}{Character. Secondary domain classification}
#' }
#' @source Facetmap psychological assessment mapping
NULL 