#' @describeIn getEdges Get Edges From a \code{MicrobiomeFunction} Object
#' @export
setMethod("getEdges", "MiCo", function(object, type) {
  # Introduce here an option to retrieve either the metabolite edges or species
  # edges.
  object@edges
})
