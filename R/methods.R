#### Show Method ####
#' Show method for MicrobialCommunity objects
#'
#' @param object An object of class MicrobialCommunity.
#' @export
setMethod("show", "MicrobialCommunity", function(object) {
  cat("MicrobialCommunity Object\n")
  cat(" - Unique microorganisms (MO):", length(unique(object@MO)), "\n")
  cat(" - Unique metabolites (met):", length(unique(object@met)), "\n")
})

#### Access Community ####
setGeneric("getCo", function(object) {
  standardGeneric("getCo")
})

setMethod("getCo", "MicrobialCommunity", function(object) {
  tibble::tibble(
    MO = object@MO,
    met = object@met,
    flux = object@flux
  )
})

#### Access MO ####
# Accessor for MO slot
#' Get MO from MicrobialCommunity object
#'
#' @param unique Liogical to toggle unique output.
#' @param object An object of class MicrobialCommunity.
#'
#' @return A character vector representing the microorganisms.
#' @export
setGeneric("getMO", function(object, unique = FALSE) {
  standardGeneric("getMO")
})

#' @param MicrobialCommunity
#'
#' @describeIn getMO Get MO from MicrobialCommunity object
#' @return A character vector representing the microorganisms.
#' @export
setMethod("getMO", "MicrobialCommunity", function(object, unique) {
  cat(
    "Microorganisms in community:\n",
    "Length:", length(object@MO), ", ",
    "unique:", length(unique(object@MO)),
    "\n", sep = "")
  t <- table(object@MO)
  for (i in 1:length(t)) {
    cat(" - ", names(t)[i], ": ", t[i], "\n", sep = "")
  }
  if (unique) {
    invisible(unique(object@MO))
  } else {
    invisible(object@MO)
  }
})


#### Access Mets ####
# Accessor for met slot
#' Get met from MicrobialCommunity object
#'
#' @param object An object of class MicrobialCommunity.
#' @return A character vector representing the metabolites.
#' @export
setGeneric("getMet", function(object) {
  standardGeneric("getMet")
})

#' @describeIn getMet Get met from MicrobialCommunity object
#' @param object An object of class MicrobialCommunity.
#' @return A character vector representing the metabolites.
#' @export
setMethod("getMet", "MicrobialCommunity", function(object) {
  object@met
})


#### Access Flux ####
# Accessor for flux slot
#' Get flux from MicrobialCommunity object
#'
#' @param object An object of class MicrobialCommunity.
#' @return A numeric vector representing the fluxes.
#' @export
setGeneric("getFlux", function(object) {
  standardGeneric("getFlux")
})

#' @describeIn getFlux Get flux from MicrobialCommunity object
#' @param object An object of class MicrobialCommunity.
#' @return A numeric vector representing the fluxes.
#' @export
setMethod("getFlux", "MicrobialCommunity", function(object) {
  object@flux
})


#### Make Alignments ####
setGeneric("align", function(object, pairwise = TRUE) {
  standardGeneric("align")
})

setMethod("align", "MicrobialCommunityAlignment", function(object, pairwise) {
  if (pairwise) { res <- pairwise_align(object@mi_co)}
  if (!pairwise) { print("To be developed.") }
  object@output_data <- res
})
