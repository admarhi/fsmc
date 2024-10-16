#' Pairwise Alignment of \code{MicrobiomeFunction} Objects
#'
#' - This method only allows for the alignment of edges which are present in both communities.
#' -
#'
#' @param coms Named list of MiCo objects.
#'
#' @return List of pairwise alignments.
#' @examples
#' ###
.pairwiseAlignment <- function(coms) {
  combs <- utils::combn(seq(length(coms)), 2)
  for (i in seq(ncol(combs))) {
    ### Simply call a multiple alignment for each pair?
  }
}
