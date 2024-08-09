#' Constructor for Microbial Community Alignment Objects
#'
#' @param ... MiCo objects to be aligned.
#' @param pairwise Logical indicating whether to perform pairwise alignment.
#' @param comment A comment to be added to the alignment.
#'
#' @export
newMiCoAl <- function(..., pairwise = FALSE, comment = NULL) {

  ### Make check that it is either multiple MiCo or a list of MiCo

  if (length(list(...)) == 1 && is.list(list(...)[[1]])) {
    coms <- list(...)[[1]]
  } else {
    coms <- list(...)
  }
  # Make list of communities
  # coms <- list(...)

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
