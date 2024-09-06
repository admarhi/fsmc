#' Constructor for Microbial Community Alignment Objects
#'
#' @param ... MiCo objects to be aligned.
#' @param pairwise Logical indicating whether to perform pairwise alignment.
#' @param comment A comment to be added to the alignment.
#'
#' @export
newMiCoAl <- function(..., pairwise = FALSE, comment = NULL) {

  if (length(list(...)) == 1 && is.list(list(...)[[1]])) {
    coms <- list(...)[[1]]
  } else {
    coms <- list(...)
  }

  # Check that all list entries are MiCo objects
  stopifnot(exprs = {
    all(lapply(coms, class) == "MiCo")
  })

  # Name the communities
  names(coms) <- sapply(coms, function(x) x@name)
  # Turn list into hash for quicker access
  coms <- hash::hash(coms)

  # Perform the desired alignment type
  ### Can be extended easily with switch in the future
  align_func <- if (pairwise) .alignPairwise else .alignMultiple

  # Perform the alignment
  alignment <- align_func(coms)
  ### Add type of the type to the alignment

  ### Alignment score here

  methods::new(
    "MiCoAl",
    communities = hash::as.list.hash(coms),
    alignment = hash::as.list.hash(alignment))
}
