#' @param object An object of class \code{MicrobiomeFunction}
#' @describeIn getCo Get the Community
#' @return A list with the community data.
#' @export
setMethod("getCo", "MicrobiomeFunction", function(object) {
  object@InputData
})