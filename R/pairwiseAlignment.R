#' @noRd
.pairwiseAlignment <- function(coms) {
  combs <- utils::combn(seq(length(coms)), 2)
  for (i in seq(ncol(combs))) {
    ### Simply call a multiple alignment for each pair?
  }
}
