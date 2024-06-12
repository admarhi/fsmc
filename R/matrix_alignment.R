#' Functional Similarity of Microbial Communities
#'
#' @param co_list List containing communities
#'
#' @return List with alignment values
#' @export
#'
#' @examples
#' #
MiCo.align.v2 <- function(co_list) {

  # Get number of communities and print warning about flux score
  if (length(co_list) > 2) message("\nFluxes only available for two comms!\n")

  # Add names if none exist
  if (is.null(names(co_list))) {
    names(co_list) <- paste0("comm_", LETTERS[seq(length(co_list))])
  }

  # Get names
  co_names <- names(co_list)

  # Initiate Lists
  neighbs <- list()
  bin_matrices <- list()
  edges_n <- list()
  edges_list <- list()
  flux_prod_j <- list()
  flux_cons_i <- list()
  flux_prod_j_eff <- list()
  flux_cons_i_eff <- list()

  # Iterate over communities
  for (co in co_names) {

    # Get neighbours of all communities
    neighbs[[co]] <- get_neighbs(co_list[[co]], silent = TRUE)

    # Get intersecting metabolites
    if (co == co_names[1]) {
      mets <- unique(co_list[[1]]$met)
    } else {
      mets <- sort(intersect(mets, unique(co_list[[co]]$met)))
    }
  }

  # Throw error if no common metabolites
  if (length(mets) == 0) stop("No common metabolites between communities.")

  ### Make nice output about which metabolites will be aligned

  # Create raw 0 matrix of dimension length(mets) * length(mets)
  raw_mat <- matrix(rep(NA, length(mets)^2), nrow = length(mets))


  # Iterate over all communities
  for (co in co_names) {

    # Set NA matrix
    bin_matrices[[co]] <- raw_mat
    edges_n[[co]] <- raw_mat
    edges_list[[co]] <- list()

    # Iterate over input mets
    for (i in 1:length(mets)) {

      # Get consumers met i
      consu_i <- neighbs[[co]]$met[[mets[i]]]$consumers

      # Iterate over output mets
      for (j in 1:length(mets)) {

        # Set NA if on diagonal
        # if (i == j) { out_list$bin_matrices[[co]][i,j] <- NA; next }
        if (i == j) next

        # Get producers of met j
        prods_j <- neighbs[[co]]$met[[mets[j]]]$producers

        # Get the intersection between prods_i and cons_i (edges)
        edges <- Reduce(intersect, list(prods_j, consu_i))

        # Set 0 and skip if no edges
        if (length(edges) == 0) { bin_matrices[[co]][i,j] <- 0; next }

        # Set binary matrix to 1
        bin_matrices[[co]][i,j] <- 1

        # Save number of edges
        edges_n[[co]][i,j] <- length(edges)

        # Save edges
        edges_list[[co]][[paste0(mets[i], "_to_", mets[j])]] <- edges

      }
    }
  }

  # Calculate the bin mat alignment
  bin_mat <- Reduce(`*`, bin_matrices)

  # Set up the output matrix for the alignment
  alignment <- bin_mat

  # Init matrices depending on options
  js_mat <- raw_mat

  for (co in co_names) {
    flux_prod_j[[co]] <- raw_mat
    flux_prod_j_eff[[co]] <- raw_mat # Effective fluxes
    flux_cons_i[[co]] <- raw_mat
    flux_cons_i_eff[[co]] <- raw_mat # Effective fluxes
  }

  # Calculations
  for (i in seq(nrow(bin_mat))) {
    for (j in seq(ncol(bin_mat))) {

      if (is.na(bin_mat[i,j]) | bin_mat[i,j] == 0) next

      # Make the edge name
      edge <- paste0(mets[i], "_to_", mets[j])

      # Get the edges
      edge_list <- purrr::map_depth(edges_list, .depth = 1, edge)

      ##### Jaccard Score #####
      # Get the intersection
      edge_intersect <- Reduce(intersect, edge_list)

      # Get the union
      edge_union <- unique(unname(unlist(edge_list)))

      # Calculate the Jaccard Score
      jaccard <- length(edge_intersect) / length(edge_union)

      # Set to 0 if NaN
      if (is.nan(jaccard)) jaccard <- 0

      # Write to Jaccard matrix
      js_mat[i,j] <- round(jaccard, 2)

      ##### Flux Differences #####
      for (co in co_names) {

        # Get the fluxes of production of met j
        flux_prod <- neighbs[[co]]$met[[mets[j]]]$prod_fluxes

        # Get the fluxes of consumption of met i
        flux_cons <- neighbs[[co]]$met[[mets[i]]]$cons_fluxes

        # Calculate the sums
        prod_j_sum <- sum(flux_prod[names(flux_prod) %in% edge_list[[co]]])
        cons_i_sum <- sum(flux_cons[names(flux_cons) %in% edge_list[[co]]])

        # Assign the sum for metabolite for each community
        flux_prod_j[[co]][i,j] <- round(prod_j_sum, 2)
        flux_cons_i[[co]][i,j] <- round(cons_i_sum, 2)

        # Calculate probabilities
        prod_j_prob <- flux_prod / sum(flux_prod)
        cons_i_prob <- flux_cons / sum(flux_cons)

        # Calculate entropy
        prod_j_ent <- 2^(-sum(prod_j_prob * log2(prod_j_prob)))
        cons_i_ent <- 2^(-sum(cons_i_prob * log2(cons_i_prob)))

        # Filtered for fluxes in edge
        prod_j_filt <- flux_prod[names(flux_prod) %in% edge_list[[co]]]
        cons_i_filt <- flux_cons[names(flux_cons) %in% edge_list[[co]]]

        # Calculate filtered probabilities
        prod_j_prob_filt <- prod_j_filt / sum(prod_j_filt)
        cons_i_prob_filt <- cons_i_filt / sum(cons_i_filt)

        # Calculate effective flux
        prod_j_eff_filt <- 2^(-sum(prod_j_prob_filt * log2(prod_j_prob_filt)))
        cons_i_eff_filt <- 2^(-sum(cons_i_prob_filt * log2(cons_i_prob_filt)))

        flux_prod_j_eff[[co]][i,j] <- prod_j_eff_filt
        flux_cons_i_eff[[co]][i,j] <- cons_i_eff_filt
      }

      ### Make this work for n numbers of comms
      fluxes_p <-
        c(flux_prod_j[[1]][i,j], flux_prod_j[[2]][i,j])
      # fluxes_c <-
      #   c(out_list$flux_cons_i[[1]][i,j], out_list$flux_cons_i[[2]][i,j]) * -1

      # Set the alignment as the minimum flux of the production of j
      alignment[i,j] <- min(fluxes_p)

    }
  }

  #### Calculate the identity score ####

  # Get the sum of the alignment
  alignment_sum <- sum(alignment, na.rm = TRUE)

  # Get the sums of the communities
  # 1. bin mat - alignment
  # 2. resulting mat * the value matrix
  # 3. sum of resulting matrix
  # 4. take the sum of these, add alignment
  # 5. divide alignment_sum by resulting value

  # Subtract the alignment from the communities' binary matrices
  minus_alig_list <- lapply(bin_matrices, `-`, e = bin_mat)


  # Multiply the reduced matrices by the production matrices
  for (co in names(co_list)) {
    minus_alig_list[[co]] * flux_prod_j[[co]]
  }




  res <- list(
    comms = co_list,
    neighbs = neighbs,
    mets = mets,
    bin_matrices = bin_matrices,
    edges_n = edges_n,
    edges = edges_list,
    bin_mat = bin_mat,
    alignment = alignment,
    js_mat = js_mat,
    flux_cons_i = flux_cons_i,
    flux_prod_j = flux_prod_j,
    flux_cons_i_eff = flux_cons_i_eff,
    flux_prod_j_eff = flux_prod_j_eff
  )

  return(res)
}

