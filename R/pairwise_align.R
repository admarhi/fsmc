#' Pairwise Alignment of Microbial Communities
#'
#' @param mi_co_list A list of microbial community objects
#'
#' @return A list of information about the alignment of the communities.
#' @export
#'
#' @examples
#' ### Todo
pairwise_align <- function(mi_co_list) {
  combs <- combn(seq(length(mi_co_list)), 2)
  for (i in seq(ncol(combs))) {
    c1 <- combs[1,i]
    c2 <- combs[2,i]
    MiCo.align.v2(mi_co_list[c(c1, c2)])
  }
}
