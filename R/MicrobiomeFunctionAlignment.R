# Save individual microbiomes with their name and bin mat
# Store the metadata in a tibble containing all info

#' @title Alignment of \code{MicrobiomeFunction} objects
#'
#' @param ... A minimum of two \code{Microbiome Function} objects.
#' @param name Character scaler specifying a name or ID for the Alignment.
#' @param min_rxn Numeric scalar giving the minimum fraction of reactions.
#' @param min_mb Numeric scalar giving the minimum fraction of microbiomes.
#' @param pairwise Boolean scalar to toggle pairwise / multiple alignment.
#'
#' @export
#'
#' @importFrom SummarizedExperiment metadata<-
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
MicrobiomeFunctionAlignment <- function(
  ...,
  name = NA_character_,
  min_rxn = 1,
  min_mb = 2,
  pairwise = FALSE
) {
  if (length(list(...)) == 1 && is.list(list(...)[[1]])) {
    coms <- list(...)[[1]]
  } else {
    coms <- list(...)
  }

  # Check that all list entries are MF objects
  stopifnot(exprs = {
    all(lapply(coms, class) == "MicrobiomeFunction")
  })

  # Name the communities
  names(coms) <- sapply(coms, function(x) x@Name)

  # Turn list into hash for quicker access
  coms <- hash::hash(coms)

  # Perform the desired alignment type
  ### Can be extended easily with switch in the future
  align_func <- if (pairwise) .pairwiseAlignment else .multipleAlignment

  ### Make trees here

  # Perform the alignment
  alignment <- align_func(coms)
  ### Add type to the alignment

  # Scoring
  score <- .scoreAlignment(
    hash::as.list.hash(alignment),
    min_rxn = min_rxn,
    min_mb = min_mb
  )

  tse <- TreeSummarizedExperiment(
    assays = list(Levels = alignment$levels_mat)
  )

  newMicrobiomeFunctionAlignment(
    tse,
    Name = name,
    Alignment = alignment,
    Score = score,
    Communities = as.list(names(coms))
  )
}
