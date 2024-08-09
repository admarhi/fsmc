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

  # Init the matrices
  bin_mat <- # Binary matrix specifying existent edges
    n_edges <- # Matrix of the number of species in the edge
    p_j_f <- # Matrix of total flux of production of j
    p_j_ef <- # Matrix of effective flux of production of j
    c_i_f <- # Matrix of total flux of consumption of i
    c_i_ef <- # Matrix of effective flux of consumption of i
    # Matrix of dimensions length(mets) x length(mets) with 0s
    matrix(
      data = 0, nrow = length(mets), ncol = length(mets),
      dimnames = list(mets, mets))

  # Calculate the other matrices' values
  for (m1 in mets) {
    # Get the consumers of m1 as char vector
    cons_m1 <- edges$metabolites[[m1]]$consumers
    for (m2 in mets) {
      # Get the producers of m2 as char vector
      prods_m2 <- edges$metabolites[[m2]]$producers
      # Get species in edge as character vector
      met_edges <- intersect(prods_m2, cons_m1)
      # Skip if no edge
      if (length(met_edges) == 0) next
      # Assign one to bin_mat if at least one edge exists
      bin_mat[m1,m2] <- 1
      # Assign the number of species in the edge to the n_edges matrix
      n_edges[m1,m2] <- length(met_edges)

      # Get the fluxes of consumption of m1
      c_m1 <- edges$metabolites[[m1]]$cons_fluxes
      # Get the fluxes of production of m2
      p_m2 <- edges$metabolites[[m2]]$prod_fluxes

      # Assign the sum of the consumption flux of m1 to the output matrix
      c_i_f[m1,m2] <- sum(c_m1[names(c_m1) %in% met_edges])
      # Assign the sum of the production flux of m2 to the output matrix
      p_j_f[m1,m2] <- sum(p_m2[names(p_m2) %in% met_edges])

      # Calculate the probabilities of the prod/cons fluxes
      c_m1_prob <- c_m1 / sum(c_m1)
      p_m2_prob <- p_m2 / sum(p_m2)

      # Calculate the effective fluxes
      c_i_ef[m1,m2] <- round(2**(-sum(c_m1_prob * log2(c_m1_prob))), 2)
      p_j_ef[m1,m2] <- round(2**(-sum(p_m2_prob * log2(p_m2_prob))), 2)
    }
  }

  # Create the community
  methods::new(
    "MiCo",
    species = species,
    metabolites = metabolites,
    fluxes = fluxes,
    edges = edges,
    bin_matrix = bin_mat,
    n_edges_matrix = n_edges,
    flux_prod_j_matrix = p_j_f,
    eff_flux_prod_j_matrix = p_j_ef,
    flux_cons_i_matrix = c_i_f,
    eff_flux_cons_i_matrix = c_i_ef,
    name = name)
}
