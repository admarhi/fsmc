#' @title Set of \code{MicrobiomeFunction} Objects
#'
#' @param name Character scalar giving the name of the set.
#'
#' @export
#'
MicrobiomeFunctionSet <- function(
    ...,
    name = NA_character_) {
  # Check that all list entries are MF objects
  stopifnot(exprs = {
    all(lapply(coms, class) == "MicrobiomeFunction")
  })





}
