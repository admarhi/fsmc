#' @title Functional Microbiome Representation based on 
#' \code{TreeSummarizedExperiment}
#' 
#' @param data a DataFrame-like object that includes columns specfiying
#' the species, metabolites and fluxes in the microbiome. The fluxes can 
#' either be weighted or unweighted (all of magnitude 1).
#' @param name a \code{character scalar} specifying the name of the Microbiome
#' 
#' @export
#' @importFrom SummarizedExperiment metadata<-
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom dplyr filter
MicrobiomeFunction <- function(data, name, ...) {
  ### Include for all columns and use str_detect
  # data <- dplyr::rename(data, met = "metabolite")
  stopifnot(exprs = {
    all(c("species", "met", "flux") %in% names(data))
  })

  # Determine whether or not the fluxes are weighted or not
  weighted <- !all(data$flux**2 == 1)

  
  edges <- findEdges(data)
  consumed <- sort(unique(data$met[data$flux < 0]))
  produced <- sort(unique(data$met[data$flux > 0]))
  all_mets <- sort(unique(data$met))
  dims <- c(length(consumed), length(produced))
  bin_mat <- matrix(rep(0, length(all_mets)**2), nrow = length(all_mets))
  n_edges <- bin_mat
  
  for (c in seq(all_mets)) {

    if (!all_mets[c] %in% consumed) next
    consumers <- edges$met_edges[[all_mets[c]]]$consumers

    for (p in seq(all_mets)) {
      if (!all_mets[p] %in% produced) next
      producers <- edges$met_edges[[all_mets[p]]]$producers
      curr_edges <- intersect(producers, consumers)
      if (length(curr_edges) == 0) next
      bin_mat[c,p] <- 1
      n_edges[c,p] <- length(curr_edges)
      if (weighted) {
        mats <- c("c_mat", "p_mat", "c_eff", "p_eff")
        for (i in mats) assign(i, bin_mat)
        c_flux <- edges$met_edges[[all_mets[c]]]$cons_fluxes
        p_flux <- edges$met_edges[[all_mets[c]]]$prod_fluxes
        c_mat[c,p] <- sum(c_flux[names(c_flux) %in% curr_edges])
        p_mat[c,p] <- sum(p_flux[names(p_flux) %in% curr_edges])
        c_prob <- c_flux / sum(c_flux)
        p_prob <- p_flux / sum(p_flux)
        c_eff[c,p] <- round(2**(-sum(c_prob * log2(c_prob))), 2)
        p_eff[c,p] <- round(2**(-sum(p_prob * log2(p_prob))), 2)
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

  rownames(assays$Binary) <- all_mets
  colnames(assays$Binary) <- all_mets
  row_data <- tibble::tibble(metabolite = all_mets)
  col_data <- tibble::tibble(metabolite = all_mets)
  
  tse <- TreeSummarizedExperiment(
    assays = assays,
    rowData = row_data,
    colData = col_data
  )

  graphs <- list(igraph::graph_from_adjacency_matrix(
    adjmatrix = bin_mat,
    mode = "directed"
  )
  )

  .MicrobiomeFunction(
    tse,
    Name = name,
    Edges = edges,
    Weighted = weighted,
    InputData = data,
    Metabolites = unique(data$met),
    Graphs = graphs
  )
}



