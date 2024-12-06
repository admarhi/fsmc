#' Multiple Alignment of \code{MicrobiomeFunction} Objects
#'
#' @param coms Named list of MiCo objects.
#'
#' @return A list containing the alignment data.
#' @importFrom MultiAssayExperiment assays
.multipleAlignment <- function(coms) {
  # Get all unique metabolites
  mets <- unique(unlist(lapply(coms, getMet)))

  # Init the level matrix
  levels_mat <- matrix(
    data = 0,
    nrow = length(mets),
    ncol = length(mets),
    dimnames = list(mets, mets)
  )

  ### Instead of naming the matrix could code metabolites to index to inc speed

  # Init the alignment hash
  al <- hash::hash()

  # Iterate over all metabolites
  for (m1 in mets) {
    for (m2 in mets) {
      # Make edge name
      edge <- paste0(m1, "_to_", m2)
      # Init edge hash in al
      al[[edge]] <-
        hash::hash(
          count = 0,
          communities = hash::hash()
        )
      # Iterate over all communities
      for (co in hash::keys(coms)) {
        # Check if the edge exists
        if (
          m1 %in% coms[[co]]@Metabolites &&
            m2 %in% coms[[co]]@Metabolites &&
            assays(coms[[co]])$Binary[m1, m2] > 0
        ) {
          # Increment counter
          levels_mat[m1, m2] <- al[[edge]]$count <- al[[edge]]$count + 1

          ### Benefit of having the matrix in addition to hash?

          # Add the community name to communities hash
          al[[edge]]$communities[[co]] <- hash::hash()
          ### Retrieve all additional relevant data
        }
      }
      # Remove community hash from edge hash if no communities
      if (length(al[[edge]]$communities) == 0) hash::del(edge, al)
    }
  }

  # Find the different levels of the alignment
  levels <-
    as.vector(levels_mat) %>%
    (function(x) x[x != 0]) %>%
    unique() %>%
    sort(decreasing = TRUE)

  al$levels <- levels
  al$levels_mat <- levels_mat

  return(al)
}
