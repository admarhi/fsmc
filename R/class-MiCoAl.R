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
        "All elements must be MiCo objects")
    }
    TRUE
  }
)

#' Constructor for Microbial Community Alignment Objects
#'
#' @param ... MiCo objects to be aligned.
#' @param pairwise Logical indicating whether to perform pairwise alignment.
#'
#' @export
MiCoAl <- function(..., pairwise = FALSE) {

  # Make list of communities
  coms <- list(...)
  # Name the communities
  names(coms) <- sapply(coms, function(x) x@name)
  # Turn list into hash for quicker access
  coms <- hash::hash(coms)

  # Perform the desired alignment type
  ### Can be extended easily with switch in the future
  align_func <- if (pairwise) pairwise_alignment else multiple_alignment

  # Perform the alignment
  alignment <- align_func(coms)

  ### Add name of the type to the alignment

  methods::new(
    "MiCoAl",
    communities = hash::as.list.hash(coms),
    alignment = hash::as.list.hash(alignment))
}
