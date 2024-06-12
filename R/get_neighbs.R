#' Find All Neighbors in a Community
#'
#' Takes a data frame describing a microbial community as input and returns a
#' list with both neighbors and ###
#'
#' @param df Data frame describing a microbial community
#' @param silent Logical to toggle output
#'
#' @return A list with neighbor data
#' @export
#'
#' @examples
#'
#' get_neighbs(exCo1)
get_neighbs <- function(df, silent = FALSE) {
  species <- unique(df$MO)
  mets <- unique(df$met)

  met_hash <- hash::hash()
  neighbs_hash <- hash::hash()

  for (m in mets) {
    producers <- df$MO[df$met == m & df$flux > 0]
    consumers <- df$MO[df$met == m & df$flux < 0]
    prod_fluxes <- df$flux[df$met == m & df$flux > 0]
    cons_fluxes <- df$flux[df$met == m & df$flux < 0]
    names(prod_fluxes) <- producers
    names(cons_fluxes) <- consumers
    if (!silent) {
      cat("--------", m, "--------\n")
      cat("Producers:", producers, "\n")
      cat("Consumers:", consumers, "\n")
    }
    met_hash[[m]] <- hash::hash(
      producers = producers,
      consumers = consumers,
      prod_fluxes = prod_fluxes,
      cons_fluxes = cons_fluxes)
  }

  for (s in species) {
    mets_in <- unique(df$met[df$MO == s & df$flux < 0])
    mets_out <- unique(df$met[df$MO == s & df$flux > 0])

    neighbs_from <- vector()
    neighbs_to <- vector()

    for (m in mets_in) neighbs_from <- c(neighbs_from, met_hash[[m]]$producers)
    for (m in mets_out) neighbs_to <- c(neighbs_to, met_hash[[m]]$consumers)

    if (!silent) {
      cat("--------", s, "--------\n")
      cat("From:", neighbs_from, "\n")
      cat("To:", neighbs_to, "\n")
    }

    neighbs_hash[[s]] <- hash::hash(from = neighbs_from, to = neighbs_to)
  }

  return(list(met = met_hash, mo = neighbs_hash))
}
