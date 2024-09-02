#' Get Metabolites
#'
#' @param object An object of class MiCo or MiCoAl
#' @param unique Logical to toggle unique output.
#'
#' @return A character vector representing the metabolites.
#' @export
setGeneric("getMet", function(object, unique = TRUE) {
  standardGeneric("getMet")
})


#' @rdname getMet
setMethod("getMet", "MiCo", function(object, unique = TRUE) {
  if (unique) return(unique(object@metabolites))
  object@metabolites
})


#' @rdname getMet
setMethod("getMet", "MiCoAl", function(object) {
  object@alignment$levels_mat %>% rownames()
})

