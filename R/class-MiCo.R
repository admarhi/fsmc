#' @title Microbial Community Class
#'
#' @description A class to represent communities of microorganisms.
#'
#' @slot species A character vector representing the microorganisms present in
#' the community.
#' @slot metabolites A character vector representing the metabolites consumed
#' and produced within the community.
#' @slot fluxes A numeric vector representing the fluxes of each metabolite in
#' the community.
#' @slot edges A list containing a hash of neighbouring metabolites and
#' neighbouring species. For neighbouring metabolites, both the ingoing and
#' outgoing flux is specified.
#' @slot bin_matrix A binary matrix representing the presence of fluxes.
#' @slot flux_matrix A matrix representing the fluxes between metabolites.
#' @slot names A string representing the name of the community.
#' @slot n_edges_matrix A matrix representing the number of species in the
#' edge between two metabolites.
#' @slot flux_prod_j_matrix A matrix representing the total flux of production
#' of j in the community.
#' @slot eff_flux_prod_j_matrix A matrix giving the effective fluxes of
#' production of j in the community.
#' @slot flux_cons_i_matrix A matrix giving the total flux of consumption of i
#' in the community.
#' @slot eff_flux_cons_i_matrix A matrix giving the effective fluxes of
#' consumption of i in the community.
#'
#' @exportClass MiCo
#'
setClass(
  "MiCo",
  slots = c(
    species = "character",
    metabolites = "character",
    fluxes = "numeric",
    edges = "list",
    bin_matrix = "matrix",
    flux_matrix = "matrix",
    n_edges_matrix = "matrix",
    flux_prod_j_matrix = "matrix",
    eff_flux_prod_j_matrix = "matrix",
    flux_cons_i_matrix = "matrix",
    eff_flux_cons_i_matrix = "matrix",
    names = "character"
  ),
  prototype = list(
    species = character(),
    metabolites = character(),
    fluxes = numeric(),
    edges = list(),
    bin_matrix = matrix(),
    flux_matrix = matrix(),
    n_edges_matrix = matrix(),
    flux_prod_j_matrix = matrix(),
    eff_flux_prod_j_matrix = matrix(),
    flux_cons_i_matrix = matrix(),
    eff_flux_cons_i_matrix = matrix(),
    names = character()
  ),
  validity = function(object) {
    if (length(object@metabolites) != length(object@fluxes)) {
      return("The number of metabolites and fluxes must be the same")
    }
    ### Build other checks
    TRUE
  }
)


#' @title Constructor Function for MiCo (Microbial Community) Objects
#'
#' @description Constructor function for MiCo objects. This function can be
#' used to create a new MiCo object from a CSV file or from vectors of
#' microorganisms, metabolites and fluxes.
#'
#' @param data A data frame/tibble or a path to a csv file storing community
#' data. Not required when using the `species`, `metabolites`, and `fluxes`
#' arguments.
#' @param name A character vector representing the name of the community.
#' @param species character vector representing the microorganisms present in
#' the community.
#' @param metabolites A character vectors representing the metabolites consumed
#' and produced within the community.
#' @param fluxes A numeric vector representing the fluxes of each metabolite in
#' the community.
#'
#' @export
MiCo <- function(
    data = NULL,
    name = NULL,
    species = character(),
    metabolites = character(),
    fluxes = numeric()) {

  if (!is.null(data)) {
    if (is.character(data) && file.exists(data)) {
      if (is.null(name)) name <- sub(".csv", "", basename(data))
      data <- tibble::as_tibble(utils::read.csv(data))
    } else if (is.data.frame(data) | tibble::is_tibble(data)) {
      if (is.null(name)) name <- deparse(substitute(data))
    }
    stopifnot(exprs = {
      all(c("species", "metabolites", "fluxes") %in% names(data))
      !is.null(name) ### Should be obsolete
    })
    species <- data$species
    metabolites <- data$metabolites
    fluxes <- data$fluxes
  } else if (
      is.null(species) |
      is.null(metabolites) |
      is.null(fluxes)) {
    stop(
      "When data is not given, 'species', 'metabolites' and 'fluxes' ",
      "must be provided.")
  }

  # Get the edges
  edges <- get_edges(tibble::tibble(
    species = species,
    metabolites = metabolites,
    fluxes = fluxes), silent = TRUE)

  # Init the matrices
  mets <- unique(metabolites)
  bin_matrix <-
    n_edges_matrix <-
    flux_prod_j_matrix <-
    eff_flux_prod_j_matrix <-
    flux_cons_i_matrix <-
    eff_flux_cons_i_matrix <-
    matrix(
      data = 0,
      nrow = length(mets),
      ncol = length(mets),
      dimnames = list(mets, mets))

  # Fill the matrices
  for (i in seq(length(mets))) {
    consumers_i <- edges$metabolites[[mets[i]]]$consumers
    for (j in seq(length(mets))) {
      producers_j <- edges$metabolites[[mets[j]]]$producers
      # met_edges <- Reduce(intersect, list(producers_j, consumers_i))
      met_edges <- intersect(producers_j, consumers_i)
      if (length(met_edges) == 0) next
      bin_matrix[i,j] <- 1
      n_edges_matrix[i,j] <- length(met_edges)
      p_flux_j <- edges$metabolites[[mets[j]]]$prod_fluxes
      c_flux_i <- edges$metabolites[[mets[i]]]$cons_fluxes
      flux_prod_j_matrix[i,j] <- sum(p_flux_j[names(p_flux_j) %in% met_edges])
      flux_cons_i_matrix[i,j] <- sum(c_flux_i[names(c_flux_i) %in% met_edges])
      ### Write the method to get the effective fluxes
    }
  }

  ### Combine this with the upper loop if works
  # Calculate the other matrices' values
  # for (m1 in mets) {
  #   consumers_m1 <- edges$metabolites[[m1]]$consumers
  #   for (m2 in mets) {
  #     producers_m2 <- edges$metabolites[[m2]]$producers
  #     met_edges <- intersect(producers_m2, consumers_m1)
  #     if (bin_matrix[m1, m2] == 0) next
  #     ### Make a function that takes the respective parts of the list and
  #     ### returns the effective flux and the total flux
  #
  #   }
  # }

  # Create the community
  methods::new(
    "MiCo",
    species = species,
    metabolites = metabolites,
    fluxes = fluxes,
    edges = edges,
    bin_matrix = bin_matrix,
    n_edges_matrix = n_edges_matrix,
    flux_prod_j_matrix = flux_prod_j_matrix,
    eff_flux_prod_j_matrix = eff_flux_prod_j_matrix,
    flux_cons_i_matrix = flux_cons_i_matrix,
    eff_flux_cons_i_matrix = eff_flux_cons_i_matrix,
    names = name)
}
