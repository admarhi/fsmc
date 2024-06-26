#' Multiple Alignment of MiCo (Microbial Community) Objects
#'
#' @param coms Named list of MiCo objects.
#'
#' @return A list containing the alignment data.
#' @export
#'
#' @examples
#' ###
multiple_alignment <- function(coms) {
  ### Check that all communities are named
  # Get all unique metabolites
  mets <- unique(unlist(lapply(coms, getMet)))

  # Init the level matrix
  levels_mat <- matrix(
    data = 0, nrow = length(mets), ncol = length(mets),
    dimnames = list(mets, mets))

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
          communities = hash::hash())
      # Iterate over all communities
      for (co in hash::keys(coms)) {
        # Check if the edge exists
        if (
          m1 %in% coms[[co]]@metabolites &&
          m2 %in% coms[[co]]@metabolites &&
          coms[[co]]@bin_matrix[m1, m2] > 0
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

  #### Alignment Score ####
  # Get the minimum flux of production of j from any community for every edge.
  base <- 5


  return(al)
}
