#' Scoring Algorithm for MiCoAl Objects
#'
#' Takes an alignment object and calculates the overall alignment score.
#'
#' @param a alignment list
#' @param min_rxn Numeric scalar giving the minimum fraction of reactions
#' @param min_mb Numeric scalar giving the minimum fraction of microbiomes
#' @importFrom purrr map keep
#' @return Tibble containing scores for every level of alignment
.scoreAlignment <- function(a, min_rxn, min_mb) {
  # Get levels vec
  l <- a$levels
  # Get levels matrix
  lm <- a$levels_mat
  # Set levels mat and levels vec to 0
  ### needs to take them as input when the alignment object is reorganised
  a$levels_mat <- a$levels <- NULL
  # Get total n reactions
  nr <- length(a)

  # Get unique sets of aligned communities
  comms <- map(a, ~ sort(names(.x$communities))) %>% unique()
  # Order them by descending length
  ordered_comms <- comms[order(-sapply(comms, length))]
  # Get total n communities
  nc <- length(unique(unlist(comms)))

  # Get the total number of alignments
  na <- length(map(ordered_comms, ~ length(.x) > 1) %>% keep(~.x))

  out_tb <- tibble::tibble(
    # n aligned communities
    n_aligned_comms = integer(na),
    aligned_comms = list(character()),
    n_aligned_rxns = integer(na),
    aligned_rxns = list(character())
  )

  # Iterate over sets of communities beginning with the longest
  for (i in seq(length(ordered_comms))) {
    # Get the length
    n <- length(ordered_comms[[i]])
    # Stop if it is the last one
    if (n == 1) break
    # Get the list of communities in the current iteration
    vec <- ordered_comms[[i]]
    # Get the edges in the current set
    edges <- a %>%
      # Filter those with few count
      keep(~ .x$count >= n) %>%
      # Check whether all communities have this edge
      map(~ all(vec %in% names(.x$communities))) %>%
      # Filter those for which it is not true
      keep(~.x)

    out_tb[i, ] <-
      list(n, list(ordered_comms[[i]]), length(edges), list(names(edges))[1])
  }

  out_tb %>%
    dplyr::mutate(
      n_total_comms = nc,
      n_total_rxns = nr,
      depth = .data$n_aligned_comms / nc,
      breadth = .data$n_aligned_rxns / nr,
      score = .data$depth * .data$breadth
    )
}
