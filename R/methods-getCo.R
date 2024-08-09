#' Get the Community
#'
#' @param object An object of class MiCo or MiCoAl
#'
#' @return A tibble with the community data.
#' @export
#'
#' @examples
#' ### Write examples for both classes
setGeneric("getCo", function(object) {
  standardGeneric("getCo")
})


#' @param object An object of class MiCo
#' @describeIn getCo Get the Community
#' @return A tibble with the community data.
#' @export
#'
#' @examples
#' ### Write examples for both classes
setMethod("getCo", "MiCo", function(object) {
  tibble::tibble(
    species = object@species,
    metabolites = object@metabolites,
    fluxes = object@fluxes
  )
})
