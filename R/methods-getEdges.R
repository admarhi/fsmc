#' Get Edges From a MiCo (Microbial Community) Object
#'
#' @param object An object of class MiCo
#' @param type Character string indicating which type of edges to return. Can
#' be either of `c("species", "metabolites")`, and defaults to `NULL.`
#' If `NULL`, both types of edges are returned.
#'
#' @return A list of edges in the community.
#' @export
setGeneric("getEdges", function(object, type = NULL) {
  standardGeneric("getEdges")
})


#' @describeIn getEdges Get Edges From a MiCo (Microbial Community) Object
#' @export
setMethod("getEdges", "MiCo", function(object, type) {
  # Introduce here an option to retrieve either the metabolite edges or species
  # edges.
  object@edges
})
