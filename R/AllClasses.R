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
#' @slot name A string representing the name of the community.
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
    n_edges_matrix = "matrix",
    flux_prod_j_matrix = "matrix",
    eff_flux_prod_j_matrix = "matrix",
    flux_cons_i_matrix = "matrix",
    eff_flux_cons_i_matrix = "matrix",
    name = "character"
  ),
  prototype = list(
    species = character(),
    metabolites = character(),
    fluxes = numeric(),
    edges = list(),
    bin_matrix = matrix(),
    n_edges_matrix = matrix(),
    flux_prod_j_matrix = matrix(),
    eff_flux_prod_j_matrix = matrix(),
    flux_cons_i_matrix = matrix(),
    eff_flux_cons_i_matrix = matrix(),
    name = character()
  ),
  validity = function(object) {
    if (length(object@metabolites) != length(object@fluxes)) {
      return("The number of metabolites and fluxes must be the same")
    }
    ### Build other checks
    TRUE
  }
)

#' @title Microbial Community Alignment Class
#'
#' @description A class to represent an alignment of multiple microbial
#' communities
#'
#' @slot communities A list of objects of type MicrobialCommunity.
#' @slot alignment A list containing the alignment data.
#' @slot comment A character string containing additional information.
#'
#' @exportClass MiCoAl
#'
setClass(
  "MiCoAl",
  slots = c(
    communities = "list",
    alignment = "list",
    comment = "character"
  ),
  prototype = list(
    communities = list(),
    alignment = list(),
    comment = character()
  ),
  validity = function(object) {
    if (!all(sapply(object@communities, is, "MiCo"))) {
      return("All elements must be MiCo objects")
    }
    TRUE
  }
)
