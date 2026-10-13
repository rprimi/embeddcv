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
"item_dic_bfi2"

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
"item_dic_facetmap" 


#' Big Five Items Dictionary (four inventories)
#'
#' Tidy item dictionary for four Big Five inventories: the BFI-2, the Big Five
#' Aspect Scales, FacetMap and SENNA. One row per item. Used in the vignettes
#' `vignette("bfi2_facetmap")` and `vignette("b5_jingle_jangle")`.
#'
#' @format A data frame with 708 rows and 9 variables:
#' \describe{
#'   \item{test}{Character. Inventory: `"bfi2"`, `"b5_aspects"`, `"facetmap"`
#'     or `"senna"`.}
#'   \item{item_num}{Integer. Item order within the inventory.}
#'   \item{coditem}{Character. Item code, unique within `test`.}
#'   \item{domain}{Character. Big Five domain (O, C, E, A, N).}
#'   \item{facet}{Character. Facet code; joins with [b5_scales] by
#'     `test` + `facet`.}
#'   \item{item_en_text}{Character. Item text in English.}
#'   \item{item_pt_text}{Character. Item text in Portuguese (BFI-2 and SENNA
#'     only).}
#'   \item{pole}{Integer. Item keying: 1 = positively keyed, 0 = reverse keyed.}
#'   \item{seman_pairs}{Integer. Semantic pair grouping (BFI-2 and SENNA).}
#' }
#' @details FacetMap domains follow the Preliminary EFA classification of
#'   Irwing, Hughes & Tokarev (2023).
#' @source Soto & John (2017) <doi:10.1037/pspp0000096>; DeYoung, Quilty &
#'   Peterson (2007) <doi:10.1037/0022-3514.93.5.880>; Irwing, Hughes &
#'   Tokarev (2023) <doi:10.1177/08902070231200919>; Primi et al. (2021), SENNA.
#' @seealso [b5_scales]
"b5_items"

#' Big Five Scales Dictionary (four inventories)
#'
#' Tidy facet-scale dictionary with the authors' definitions for the BFI-2,
#' Big Five Aspect Scales, FacetMap and SENNA. One row per facet scale.
#'
#' @format A data frame with 120 rows and 11 variables:
#' \describe{
#'   \item{test}{Character. Inventory (see [b5_items]).}
#'   \item{domain}{Character. Big Five domain (O, C, E, A, N).}
#'   \item{domain_sec}{Character. Secondary domain(s), when reported.}
#'   \item{facet}{Character. Facet code; joins with [b5_items].}
#'   \item{facet_name}{Character. Facet name.}
#'   \item{definition}{Character. Facet definition in English.}
#'   \item{facet_name_pt}{Character. Facet name in Portuguese (FacetMap only).}
#'   \item{definition_pt}{Character. Definition in Portuguese (FacetMap only).}
#'   \item{n_items}{Integer. Number of items in [b5_items]; 0 for seven
#'     FacetMap facets without items.}
#'   \item{note}{Character. Notes on the definition or classification.}
#'   \item{source}{Character. Source of the definition.}
#' }
#' @source See [b5_items].
#' @seealso [b5_items]
"b5_scales"
