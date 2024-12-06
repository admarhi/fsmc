#' Return Species in a Microbiome
#'
#' @export
setGeneric("getSpecies", function(object) {
  standardGeneric("getSpecies")
})

#' Get Metabolites
#'
#' @param object a \code{MicrobiomeFunction} or
#' \code{MicrobiomeFunctionAlignment} object
#'
#' @return A character vector representing the metabolites.
#' @export
setGeneric("getMet", function(object) {
  standardGeneric("getMet")
})

#' Get Edges From a \code{MicrobiomeFunction} Object
#'
#' @param object a \code{MicrobiomeFunction} object
#'
#' @return A list of edges in the community.
#' @export
setGeneric("getEdges", function(object) {
  standardGeneric("getEdges")
})

#' Get the Community
#'
#' Returns the community of a single \code{MicrobiomeFunction} object in a \
#' tibble format or a list of communities in tibble format for
#' \code{MicrobiomeFunctionAlignment} objects.
#'
#' @export
#'
#' @examples
#' c1 <- newMiCo(ac_A1R12_1)
#' c2 <- newMiCo(ac_A1R12_2)
#' a <- newMiCoAl(c1, c2)
#'
#' getCo(c1)
#' getCo(a)
setGeneric("getCo", function(object) {
  standardGeneric("getCo")
})
