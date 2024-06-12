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
    names = "character"
  ),
  prototype = list(
    species = character(),
    metabolites = character(),
    fluxes = numeric(),
    edges = list(),
    bin_matrix = matrix(),
    flux_matrix = matrix(),
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
#' @description Constructor function for MiCo objects. This function can be used
#' to create a new MiCo object from a CSV file or from vectors of
#' microorganisms, metabolites and fluxes.
#'
#' @param path Path to a csv file storing community data. Not required when
#' using MO, met, flux to specify the data.
#' @param name A character vector representing the name of the community.
#' @param species character vector representing the microorganisms present in the
#' community.
#' @param metabolites A character vectors representing the metabolites consumed and
#' produced within the community.
#' @param fluxes A numeric vector representing the fluxes of each metabolite in
#' the community.
#'
#' @export
MiCo <- function(
    path = NULL,
    name = NULL,
    species = character(),
    metabolites = character(),
    fluxes = numeric()) {

  if (!is.null(path)) {

    ### Build for other file formats.
    ### This should be a read_community function

    # Read data from CSV file
    df <- utils::read.csv(file = path)

    # Extract columns assuming the CSV is in order "MO", "met", "flux"
    species <- df[,2] ### Check for order of columns
    metabolites <- df[,1] ### Check for order of columns
    fluxes <- df[,3] ### Check for order of columns

    if (is.null(name)) name <- sub(".csv", "", basename(path))
  }

  # Check if it is possible to create the community
  if (is.null(path) &
      is.null(species) |
      is.null(metabolites) |
      is.null(fluxes)) {
    stop(
      "When path is not given, 'species', 'metabolites' and 'fluxes' ",
      "must be provided.")
  }

  # Get the edges
  edges <- get_edges(tibble::tibble(
    species = species,
    metabolites = metabolites,
    fluxes = fluxes), silent = TRUE)

  # Get the binary matrix
  mets <- unique(metabolites)
  bin_matrix <- matrix(0, nrow = length(mets), ncol = length(mets))



  # Create the community
  methods::new(
    "MiCo",
    species = species,
    metabolites = metabolites,
    fluxes = fluxes,
    edges = edges,
    bin_matrix = bin_matrix,
    names = name)


  # Calculate the edges




}
