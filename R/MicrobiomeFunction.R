#' @title Functional Microbiome Representation based on 
#' \code{TreeSummarizedExperiment}
#' 
#' @param data a DataFrame-like object that includes columns specfiying
#' the species, metabolites and fluxes in the microbiome. The fluxes can 
#' either be directional (all of magnitude 1)
#' @param name a \code{character scalar} specifying the name of the Microbiome
#' 
#' @export
#' @importFrom SummarizedExperiment metadata<-
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom dplyr filter
MicrobiomeFunction <- function(data, name, ...) {
  
  stopifnot(exprs = {
    all(c("species", "met", "flux") %in% names(data))
  })

  directional <- all(data$flux**2 == 1)
  edges <- findEdges(data)
  consumed <- sort(unique(data$met[data$flux < 0]))
  produced <- sort(unique(data$met[data$flux > 0]))
  dims <- c(length(consumed), length(produced))
  bin_mat <- matrix(rep(0, dims[1] * dims[2]), nrow = dims[1])
  n_edges <- bin_mat
  
  for (c in seq(consumed)) {
    consumers <- edges$met_edges[[consumed[c]]]$consumers
    for (p in seq(produced)) {
      producers <- edges$met_edges[[produced[p]]]$producers
      curr_edges <- intersect(producers, consumers)
      if (length(curr_edges) == 0) next
      bin_mat[c,p] <- 1
      n_edges[c,p] <- length(curr_edges)
      if (!directional) {
        mats <- c("c_mat", "p_mat", "c_eff", "p_eff")
        for (i in mats) assign(i, bin_mat)
        c_flux <- edges$met_edges[[consumed[c]]]$cons_fluxes
        p_flux <- edges$met_edges[[produced[c]]]$prod_fluxes
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

  
  if (directional) assays <- list(Binary = bin_mat, nEdges = n_edges)
    
  rownames(assays$Binary) <- consumed
  colnames(assays$Binary) <- produced
  row_data <- tibble::tibble(metabolite = consumed)
  col_data <- tibble::tibble(metabolite = produced)
  
  tse <- TreeSummarizedExperiment(
    assays = assays,
    rowData = row_data,
    colData = col_data
  )
    
  md <- list(directional = directional)
  metadata(tse) <- list(Edges = findEdges(data))

  .MicrobiomeFunction(
    tse,
    Name = name)
}

# Helper Function to get the fluxes from edges if !directional
.getFlux <- function(e) {
  e
}

