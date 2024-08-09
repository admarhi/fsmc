#' Generate Random Synthetic Microbial Communities
#'
#' A function that creates synthetic data suitable for demonstration purposes of the `fsmc` package.
#'
#' @param n_species Number of species in the community
#' @param max_met Maximum number of metabolites in the communities
#' @param scale_fac Scaling factor
#' @param seed Seed for reproducibility
#' @param dead_ends Logical value to toggle dead ends in data
#' @param MiCo Logical value to toggle return of MiCo object or tibble.
#' @param name Character string giving the desired name of the community.
#'
#' @return List with `n_co` number of communities.
#' @export
#'
#' @examples
#' makeSynMiCo("Ex. Community", n_species = 5, max_met = 10)
makeSynMiCo <- function(
    name,
    n_species,
    max_met,
    scale_fac = 2,
    seed = FALSE,
    dead_ends = FALSE,
    MiCo = TRUE) {

  r_names <- function(n = 5000) {
    a <- do.call(paste0, replicate(3, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%03d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }

  species_names <- r_names(n_species*scale_fac)
  met_vec <- paste0("met", 1:max_met)

  if (!seed) seed <- sample(1:1000, 1)
  set.seed(seed)

  species <- vector()
  mets <- vector()
  fluxes <- vector()

  # Get sample of species for the community
  species_names <- sample(species_names, size = n_species, replace = FALSE)

  for (i in seq_along(species_names)) {
    t_met <- sample(2:max_met, 1)
    species_mets <- sample(met_vec, t_met)
    species <- c(species, rep(species_names[i], t_met))
    t_vals <- vector()
    for (m in species_mets) {
      mets <- c(mets, m)
      t_vals <- c(t_vals, stats::rnorm(1, mean = 0, sd = 3))
    }
    if (!dead_ends & (all(t_vals < 0) | all(t_vals > 0))) {
      to_change <- sample(1:length(t_vals), 1)
      t_vals[to_change] <- t_vals[to_change] * -1
    }
    fluxes <- c(fluxes, t_vals)
  }

  community <- tibble::tibble(
    species = species,
    metabolites = mets,
    fluxes = fluxes)

  if (!MiCo) return(community)

  newMiCo(
    species = species,
    metabolites = mets,
    fluxes = fluxes,
    name = name)
}


