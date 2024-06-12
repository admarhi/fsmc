#' @title MicrobialCommunityAlignment-class
#' @description A class to represent an alignment of multiple microbial
#' communities
#'
#' @slot communities A list of objects of type MicrobialCommunity.
#' @slot output A list containing the alignment data.
#'
#' @exportClass MiCoAl
setClass(
  "MiCoAl",
  slots = c(
    communities = "list",
    output = "list"
  ),
  prototype = list(
    communities = list(),
    output = list()
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
#' @param communities A list of of objects of type MiCo
#'
#' @export
MiCoAl <- function(communities) {
  methods::new(
    "MiCoAl",
    communities = communities)
}
