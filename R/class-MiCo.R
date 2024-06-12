#### Class ####
#' @title MicrobialCommunity-class
#' @description A class to represent communities of microorganisms.
#'
#' @slot MO A character vector representing the microorganisms present in the
#' community.
#' @slot met A character vectors representing the metabolites consumed and
#' produced within the community.
#' @slot flux A numeric vector representing the fluxes of each metabolite in
#' the community.
#'
#' @exportClass MiCo
setClass(
  "MiCo",
  slots = c(
    MO = "character",
    met = "character",
    flux = "numeric",
    neighbours = "list",
    edges = "list"
  ),
  prototype = list(
    MO = character(),
    met = character(),
    flux = numeric(),
    neighbours = list(),
    edges = list()
  ),
  validity = function(object) {
    if (length(object@met) != length(object@flux)) {
      return("The number of metabolites and fluxes must be the same")
    }
    ### Build other checks
    TRUE
  }
)


#### Constructor ####
#' Constructor Function for MicrobialCommunity Objects
#'
#' @param path Path to a csv file storing community data. Not required when
#' using MO, met, flux to specify the data.
#' @param MO character vector representing the microorganisms present in the
#' community.
#' @param met A character vectors representing the metabolites consumed and
#' produced within the community.
#' @param flux A numeric vector representing the fluxes of each metabolite in
#' the community.
#'
#' @export
MiCo <- function(
    path = NULL, MO = character(), met = character(), flux = numeric()) {

  if (!is.null(path)) {
    ### Needs to be amended for other file formats.
    # Read data from CSV file
    df <- utils::read.csv(file = path)
    # Extract columns assuming the CSV is in order "MO", "met", "flux"
    MO <- df[,2]
    met <- df[,1]
    flux <- df[,3]
  }

  if (is.null(path) & is.null(MO) | is.null(met) | is.null(flux)) {
    stop("When path is not given, 'MO', 'met' and 'flux' must be provided.")
  }
  methods::new(
    "MiCo",
    MO = MO,
    met = met,
    flux = flux)
}
