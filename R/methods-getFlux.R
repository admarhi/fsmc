#' Get Fluxes from MiCo (Microbial Community) Object
#'
#' @param object An object of class MiCo.
#' @return A numeric vector representing the fluxes.
#' @export
setGeneric("getFlux", function(object) {
  standardGeneric("getFlux")
})

#' @describeIn getFlux Get flux from MicrobialCommunity object
#' @param object An object of class MicrobialCommunity.
#' @return A numeric vector representing the fluxes.
#' @export
setMethod("getFlux", "MiCo", function(object) {
  object@fluxes
})
