#' Get Metabolites from MiCo object
#'
#' @param object An object of class MiCo
#' @param unique Logical to toggle unique output.
#'
#' @return A character vector representing the metabolites.
#' @export
setGeneric("getMet", function(object, unique = FALSE) {
  standardGeneric("getMet")
})


#' @rdname getMet
setMethod("getMet", "MiCo", function(object, unique) {
  if (unique) return(unique(object@metabolites))
  object@metabolites
})


#' @rdname getMet
setMethod("getMet", "MiCoAl", function(object) {
  object@alignment$levels_mat %>% rownames()
})
