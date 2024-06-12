#' Show method for MicrobialCommunity objects
#'
#' @param object An object of class MiCo
#' @export
setMethod("show", "MiCo", function(object) {
  cat("MiCo (MicrobialCommunity) Object\n")
  cat(" - Unique microorganisms (MO):", length(unique(object@MO)), "\n")
  cat(" - Unique metabolites (met):", length(unique(object@met)), "\n")
})
