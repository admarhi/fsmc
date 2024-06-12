#' @title Microbial Community Alignment Class
#'
#' @description A class to represent an alignment of multiple microbial
#' communities
#'
#' @slot communities A list of objects of type MicrobialCommunity.
#' @slot alignment A list containing the alignment data.
#'
#' @exportClass MiCoAl
#'
setClass(
  "MiCoAl",
  slots = c(
    communities = "list",
    alignment = "list"
  ),
  prototype = list(
    communities = list(),
    alignment = list()
  ),
  validity = function(object) {
    if (!all(sapply(object@communities, is, "MiCo"))) {
      return(
        "All elements of 'hypergraphs' must be MiCo objects")
    }
    TRUE
  }
)

#' Constructor for Microbial Community Alignment Objects
#'
#' @param ... Any number of MiCo objects to be aligned.
#' @param pairwise Logical indicating whether to perform pairwise alignment.
#'
#' @export
MiCoAl <- function(..., pairwise = FALSE) {
  communities <- list(...)
  names(communities) <- sapply(communities, function(x) x@names)
  methods::new(
    "MiCoAl",
    communities = communities)
}
