#' @param object An object of class MiCo
#' @describeIn getSpecies Get Species From a \code{MicrobiomeFunction} Object
#' @return A character vector representing the microorganisms.
setMethod("getSpecies", "MiCo", function(object) {
  cat(
    length(unique(object@species)),
    " microorganisms in community ", object@name, ":\n", sep = "")
  cat(paste0("  - ", unique(object@species), collapse = "\n"))
  invisible(unique(object@species))
})


#' @param object An object of class MiCoAl
#' @describeIn getSpecies Get Species From a \code{MicrobiomeFunction} Object
#' @return A character vector representing the microorganisms.
setMethod("getSpecies", "MiCoAl", function(object) {
  species <- getCo(object) %>% purrr::map(~.x$species) %>% unlist() %>% unique()
  cat(stringr::str_glue(
    "{length(unique(species))} species in alignment.\n",
    "{paste0(species, collapse = ', ')}"
  ))
  invisible(species)
})

