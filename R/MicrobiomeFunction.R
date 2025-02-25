#' @title Functional Microbiome Representation based on
#' \code{TreeSummarizedExperiment}
#'
#' @param data a DataFrame-like object that includes columns specfiying
#' the species, metabolites and fluxes in the microbiome. The fluxes can
#' either be weighted or unweighted (all of magnitude 1).
#' @param name a character scalar specifying the name of the Microbiome
#' @param species_col Character scalar specfiying the name of the species
#' column, defaults to 'spec'.
#' @param metabolite_col Character scalar specifying the name of the metabolite
#' column, defaults to 'met'.
#' @param flux_col Character scalar specifying the name of the flux column,
#' defaults to 'flux'.
#' @param ... Additional arguments to be passed to the constructor.
#'
#' @export
#'
#' @importFrom SummarizedExperiment metadata<-
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom tibble tibble
#' @importFrom dplyr filter
MicrobiomeFunction <- function(
  data,
  name = NA_character_,
  species_col = "species",
  metabolite_col = "met",
  flux_col = "flux",
  ...
) {
  ### Include checkfor all columns and use str_detect
  ### Build the renaming of the columns

  data <- data |>
    dplyr::rename(
      species = {{ species_col }},
      met = {{ metabolite_col }},
      flux = {{ flux_col }}
    )

  stopifnot(exprs = {
    all(c("species", "met", "flux") %in% names(data))
  })

  # Determine whether or not the fluxes are weighted or not
  weighted <- !all(data$flux**2 == 1)

  edges <- findEdges(data)

  # Get metabolites
  all_met <- sort(unique(data$met))
  con_met <- sort(unique(data$met[data$flux < 0]))
  pro_met <- sort(unique(data$met[data$flux > 0]))

  # Set row and col data
  row_data <- col_data <- tibble(metabolite = all_met)

  # Set up the basic matrices
  bin_mat <- matrix(rep(0, length(all_met)**2), nrow = length(all_met))
  n_edges <- bin_mat

  ### Recode the data to facilitate the

  for (c in seq(all_met)) {
    if (!all_met[c] %in% con_met) next
    consumers <- edges$met_edges[[all_met[c]]]$consumers

    for (p in seq(all_met)) {
      if (!all_met[p] %in% pro_met) next
      producers <- edges$met_edges[[all_met[p]]]$producers
      curr_edges <- intersect(producers, consumers)
      if (length(curr_edges) == 0) next
      bin_mat[c, p] <- 1
      n_edges[c, p] <- length(curr_edges)
      if (weighted) {
        mats <- c("c_mat", "p_mat", "c_eff", "p_eff")
        for (i in mats) assign(i, bin_mat)
        c_flux <- edges$met_edges[[all_met[c]]]$cons_fluxes
        p_flux <- edges$met_edges[[all_met[c]]]$prod_fluxes
        c_mat[c, p] <- sum(c_flux[names(c_flux) %in% curr_edges])
        p_mat[c, p] <- sum(p_flux[names(p_flux) %in% curr_edges])
        c_prob <- c_flux / sum(c_flux)
        p_prob <- p_flux / sum(p_flux)
        c_eff[c, p] <- round(2**(-sum(c_prob * log2(c_prob))), 2)
        p_eff[c, p] <- round(2**(-sum(p_prob * log2(p_prob))), 2)
        assays <- list(
          Binary = bin_mat,
          nEdges = n_edges,
          Consumption = c_mat,
          Production = p_mat,
          EffectiveConsumption = c_eff,
          EffectiveProduction = p_eff
        )
      }
    }
  }

  if (!weighted) assays <- list(Binary = bin_mat, nEdges = n_edges)

  ### Filter all rows and columns where all() == 0 from the assays
  ### Check that the expected number of cols/rows were filtered
  ### and all match the expected dimensions

  # rownames(assays$Binary) <- all_met
  # colnames(assays$Binary) <- all_met

  tse <- TreeSummarizedExperiment(
    assays = assays,
    rowData = row_data,
    colData = col_data
  )

  graphs <- list(
    igraph::graph_from_adjacency_matrix(
      adjmatrix = bin_mat,
      mode = "directed"
    )
  )

  newMicrobiomeFunction(
    tse,
    Name = name,
    Edges = edges,
    Weighted = weighted,
    InputData = data,
    Metabolites = unique(data$met),
    Graphs = graphs
  )
}
