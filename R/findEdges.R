#' Get All Edges of a Community
#'
#' Takes a tibble describing a microbial community as input and returns a
#' list with two hashs, one giving the edges between metabolites and the other
#' between species.
#'
#' @param tb Tibble describing a microbial community
#' @param silent Logical to toggle output
#'
#' @return A list of two hashs, one for edges between metabolites and one for
#' species.
#' @export
#'
#' @examples
#' findEdges(ac_A1R12_1)
findEdges <- function(tb, silent = FALSE) {

  ### Could delete the silent option
  ### Check that the columns are in the right order and correct names
  ### Checks that the columns are of the expected types

  names(tb) <- c("species", "met", "flux")

  species <- unique(tb$species)
  metabolites <- unique(tb$met)

  met_hash <- hash::hash()
  species_hash <- hash::hash()

  for (m in metabolites) {
    producers <- tb$species[tb$met == m & tb$flux > 0]
    consumers <- tb$species[tb$met == m & tb$flux < 0]
    prod_fluxes <- tb$flux[tb$met == m & tb$flux > 0]
    cons_fluxes <- tb$flux[tb$met == m & tb$flux < 0]
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

  ### Add the flux to the output
  for (s in species) {
    metabolites_in <- unique(tb$met[tb$species == s & tb$flux < 0])
    metabolites_out <- unique(tb$met[tb$species == s & tb$flux > 0])

    species_from <- vector()
    species_to <- vector()

    for (m in metabolites_in) {
      species_from <- c(species_from, met_hash[[m]]$producers)
    }

    for (m in metabolites_out) {
      species_to <- c(species_to, met_hash[[m]]$consumers)
    }

    if (!silent) {
      cat("--------", s, "--------\n")
      cat("From:", species_from, "\n")
      cat("To:", species_to, "\n")
    }

    species_hash[[s]] <- hash::hash(from = species_from, to = species_to)
  }

  return(list(metabolites = met_hash, species = species_hash))
}
