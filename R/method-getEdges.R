#' Get Edges From a MiCo (Microbial Community) Object
#'
#' @param object An object of class MiCo
#'
#' @return A list of edges in the community.
#' @export
setGeneric("getEdges", function(object, unique = FALSE) {
  standardGeneric("getEdges")
})


#' @describeIn getEdges Get Edges From a MiCo (Microbial Community) Object
#' @export
setMethod("getEdges", "MiCo", function(object, unique) {
  object@edges
})
