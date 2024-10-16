#' @describeIn getEdges Get Edges From a \code{MicrobiomeFunction} Object
#' @export
setMethod("getEdges", "MicrobiomeFunction", function(object) {
  object@Edges
})
