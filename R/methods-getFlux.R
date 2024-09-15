#' @describeIn getFlux Get flux from MicrobialCommunity object
#' @param object An object of class MicrobialCommunity.
#' @return A numeric vector representing the fluxes.
#' @export
setMethod("getFlux", "MiCo", function(object) {
  object@fluxes
})
