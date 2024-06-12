#' Get Metabolites from MiCo object
#'
#' @param object An object of class MiCo
#' @return A character vector representing the metabolites.
#' @export
setGeneric("getMet", function(object) {
  standardGeneric("getMet")
})


#' @describeIn getMet Get met from MicrobialCommunity object
#' @param object An object of class MicrobialCommunity.
#' @return A character vector representing the metabolites.
#' @export
setMethod("getMet", "MiCo", function(object) {
  object@metabolites
})
