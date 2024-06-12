#' Align Microbial Community Objects
#'
#' @param object A list (?) of objects of type MiCo
#' @param pairwise Logical to toggle pairwise alignment
#'
#' @return An overview over the alignment of two microbial communities.
#' @export
#'
#' @examples
#' ### Todo
setGeneric("align", function(object, pairwise = TRUE) {
  standardGeneric("align")
})


#' @describeIn align Align Microbial Community Objects
#' @export
setMethod("align", "MiCoAl", function(object, pairwise) {
  if (pairwise) { res <- pairwise_align(object@mi_co)}
  if (!pairwise) { print("To be developed.") }
  object@output_data <- res
})
