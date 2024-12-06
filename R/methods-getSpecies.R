#' @describeIn getSpecies Return Species in a Microbiome
#' @param object a \code{MicrobiomeFunction} object
#' @return A character vector representing the microorganisms.
setMethod("getSpecies", "MicrobiomeFunction", function(object) {
  cat(
    length(unique(object@InputData$species)),
    " microorganisms in community ", object@Name, ":\n",
    sep = ""
  )
  cat(paste0("  - ", unique(object@InputData$species), collapse = "\n"))
  invisible(unique(object@InputData$species))
})


#' @describeIn getSpecies Return Species in a Microbiome
#' @param object a \code{MicrobiomeFunctionAlignment} Object
#' @return A character vector representing the microorganisms.
setMethod("getSpecies", "MicrobiomeFunctionAlignment", function(object) {
  ### ToDo
})
