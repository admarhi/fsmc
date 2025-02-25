#' Get All Edges of a Community
#'
#' Takes a tibble describing a microbial community as input and returns a
#' list with two hashs, one giving the edges between metabolites and the other
#' between species.
#'
#' @param tb Tibble describing a microbial community
#'
#' @return A hash containing the edges of the community. Returns a list of
#' two hashs, one for edges between metabolites and one for species.
#' @export
#'
#' @examples
#' # findEdges(ac_A1R12_1)
findEdges <- function(tb) {
  ### This should be turned into an on-demand functino that is only called when
  ### we need info on a specific edge. Calling it by default is very expensive.

  stopifnot(exprs = {
    all(c("species", "met", "flux") %in% names(tb))
  })

  metabolites <- unique(tb$met)
  met_hash <- hash::hash()
  for (m in metabolites) {
    producers <- tb$species[tb$met == m & tb$flux > 0]
    consumers <- tb$species[tb$met == m & tb$flux < 0]
    prod_fluxes <- tb$flux[tb$met == m & tb$flux > 0]
    cons_fluxes <- tb$flux[tb$met == m & tb$flux < 0]
    names(prod_fluxes) <- producers
    names(cons_fluxes) <- consumers
    met_hash[[m]] <- hash::hash(
      producers = producers,
      consumers = consumers,
      prod_fluxes = prod_fluxes,
      cons_fluxes = cons_fluxes
    )
  }

  species <- unique(tb$species)
  species_hash <- hash::hash()
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

    species_hash[[s]] <- hash::hash(from = species_from, to = species_to)
  }

  list(met_edges = met_hash, species_edges = species_hash)
}
