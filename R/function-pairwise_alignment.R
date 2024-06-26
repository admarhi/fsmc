#' Pairwise Alignment of MiCo (Microbial Community) Objects
#'
#'
#' @param coms Named list of MiCo objects.
#'
#' @return List of pairwise alignments.
#' @export
#' @examples
#' ###
pairwise_alignment <- function(coms) {
  combs <- utils::combn(seq(length(coms)), 2)
  for (i in seq(ncol(combs))) {
    ### Simply call a multiple alignment for each pair?
  }
}
