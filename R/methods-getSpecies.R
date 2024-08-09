#' Get Species From a MiCo (Microbial Community) Object
#'
#' @param unique Logical to toggle unique output.
#' @param object An object of class MiCo
#'
#' @return A character vector representing the microorganisms.
#' @export
setGeneric("getSpecies", function(object, unique = FALSE) {
  standardGeneric("getSpecies")
})


#' @param object An object of class MiCo
#' @param unique Logical to toggle unique output.
#' @describeIn getSpecies Get Species From a MiCo (Microbial Community) Object
#' @return A character vector representing the microorganisms.
#' @export
setMethod("getSpecies", "MiCo", function(object, unique) {
  cat(
    "Microorganisms in community:\n",
    "Length:", length(object@species), ", ",
    "unique:", length(unique(object@species)),
    "\n", sep = "")
  t <- table(object@species)
  for (i in 1:length(t)) {
    cat(" - ", names(t)[i], ": ", t[i], "\n", sep = "")
  }
  if (unique) {
    invisible(unique(object@species))
  } else {
    invisible(object@species)
  }
})



## For the MiCoAl method, this could retrieve the level of the alignment and
## return the species present.
