#' Get MO from c object
#'
#' @param unique Liogical to toggle unique output.
#' @param object An object of class MiCo
#'
#' @return A character vector representing the microorganisms.
#' @export
setGeneric("getMO", function(object, unique = FALSE) {
  standardGeneric("getMO")
})


#' @param object An object of class MiCo
#' @param unique Logical to toggle unique output.
#' @describeIn getMO Get MO from MicrobialCommunity object
#' @return A character vector representing the microorganisms.
#' @export
setMethod("getMO", "MiCo", function(object, unique) {
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
