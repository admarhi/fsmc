#' Get Species From a \code{MicrobiomeFunction} Object
#'
#' @export
setGeneric("getSpecies", function(object) {
  standardGeneric("getSpecies")
})

#' Get Metabolites
#'
#' @param object An object of class MiCo or MiCoAl
#' @param unique Logical to toggle unique output.
#'
#' @return A character vector representing the metabolites.
#' @export
setGeneric("getMet", function(object, unique = TRUE) {
  standardGeneric("getMet")
})

#' Get Fluxes from \code{MicrobiomeFunction} Object
#'
#' @param object An object of class MiCo.
#' @return A numeric vector representing the fluxes.
#' @export
setGeneric("getFlux", function(object) {
  standardGeneric("getFlux")
})

#' Get Edges From a \code{MicrobiomeFunction} Object
#'
#' @param object An object of class MiCo
#' @param type Character string indicating which type of edges to return. Can
#' be either of `c("species", "metabolites")`, and defaults to `NULL.`
#' If `NULL`, both types of edges are returned.
#'
#' @return A list of edges in the community.
#' @export
setGeneric("getEdges", function(object, type = NULL) {
  standardGeneric("getEdges")
})

#' Get the Community
#'
#' Returns the community of a single MiCo object in a tibble format or a list
#' of communities in tibble format for MiCoAl objects.
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