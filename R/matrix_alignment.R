#' Functional Similarity of Microbial Communities
#'
#' @param co_list List containing communities
#' @param edges_n Toggle for number of edges output
#' @param edges_js Toggle Jaccard Score Calculation
#' @param flux_diff Get the difference of fluxes
#'
#' @return List with alignment values
#' @export
#'
#' @examples
#' #
MiCo.align <- function(
    co_list,
    edges_n = TRUE,
    edges_js = TRUE,
    flux_diff = TRUE) {

  # Create output list
  out_list <- list()

  # Get number of communities
  n_co <- length(co_list)

  # Print warning about flux score
  if (n_co > 2) message("\nFluxes only available for first two comms!\n")

  # Get names
  co_names <- names(co_list)

  # Save communities in out_list
  out_list$comms <- co_list

  # Iterate over communities
  for (co in co_names) {

    # Get neighbors of all communities
    out_list$neighbs[[co]] <- get_neighbs(out_list$comms[[co]], silent = TRUE)

    # Get intersecting metabolites
    if (co == co_names[1]) {
      mets <- unique(out_list$comms[[1]]$met)
    } else {
      mets <- sort(intersect(mets, unique(out_list$comms[[co]]$met)))
    }
  }

  # Throw error if no common metabolites
  if (length(mets) == 0) stop("No common metabolites between communities.")

  ### Make nice output about which metabolites will be aligned

  # Add mets to output
  out_list$mets <- mets

  ### Add list of MO to output

  # Create raw 0 matrix of dimension length(mets) * length(mets)
  raw_mat <- matrix(rep(NA, length(mets)^2), nrow = length(mets))

  # Iterate over all communities
  for (co in co_names) {

    # Set 0 matrix
    out_list$bin_matrices[[co]] <- raw_mat
    out_list$edges_n[[co]] <- raw_mat
    out_list$edges[[co]] <- list()

    # Iterate over input mets
    for (i in 1:length(mets)) {
      # Get consumers met i
      consu_i <- out_list$neighbs[[co]]$met[[mets[i]]]$consumers

      # Iterate over output mets
      for (j in 1:length(mets)) {

        # Set NA if on diagonal
        # if (i == j) { out_list$bin_matrices[[co]][i,j] <- NA; next }
        if (i == j) next

        # Get producers of met j
        prods_j <- out_list$neighbs[[co]]$met[[mets[j]]]$producers

        # Get the intersection between prods_i and cons_i (edges)
        edges <- Reduce(intersect, list(prods_j, consu_i))

        # Skip if no edges
        if (length(edges) == 0) {
          out_list$bin_matrices[[co]][i,j] <- 0
          # out_list$edges_n[[co]][i,j] <- 0
          next
        }

        # Set binary matrix to 1
        out_list$bin_matrices[[co]][i,j] <- 1

        # Save number of edges
        out_list$edges_n[[co]][i,j] <- length(edges)

        # Save edges
        out_list$edges[[co]][[paste0(mets[i], "_to_", mets[j])]] <- edges
      }
    }
  }

  # Calculate the bin mat alignment
  bin_mat <- Reduce(`*`, out_list$bin_matrices)
  out_list$bin_mat <- bin_mat

  # Set up the output matrix for the alignment
  out_list$alignment <- bin_mat

  # Check if additional required
  if (all(!edges_js, !flux_diff, !edges_n)) return(out_list)

  # Init matrices depending on options
  if (edges_js) out_list$js_mat <- raw_mat

  if (flux_diff) {
    for (co in co_names) {
      out_list$flux_prod_j[[co]] <- raw_mat
      out_list$flux_prod_j_eff[[co]] <- raw_mat # Effective fluxes
      out_list$flux_cons_i[[co]] <- raw_mat
      out_list$flux_cons_i_eff[[co]] <- raw_mat # Effective fluxes
    }
    out_list$flux_prod_j_abs_diff <- raw_mat
    out_list$flux_prod_j_rel_diff <- raw_mat
    out_list$flux_cons_i_abs_diff <- raw_mat
    out_list$flux_cons_i_rel_diff <- raw_mat
  }

  # Calculations
  for (i in seq(nrow(bin_mat))) {
    for (j in seq(ncol(bin_mat))) {

      if (is.na(bin_mat[i,j]) | bin_mat[i,j] == 0) next

      # Make the edge name
      edge <- paste0(mets[i], "_to_", mets[j])

      # Get the edges
      edge_list <- purrr::map_depth(out_list$edges, .depth = 1, edge)

      ##### Jaccard Score #####
      if (edges_js) {

        # Get the intersection
        edge_intersect <- Reduce(intersect, edge_list)

        # Get the union
        edge_union <- unique(unname(unlist(edge_list)))

        # Calculate the Jaccard Score
        jaccard <- length(edge_intersect) / length(edge_union)

        # Set to 0 if NaN
        if (is.nan(jaccard)) jaccard <- 0

        # Write to Jaccard matrix
        out_list$js_mat[i,j] <- jaccard

        # Round the Jaccard matrix
        out_list$js_mat <- round(out_list$js_mat, 2)
      }

      ##### Flux Differences #####
      if (flux_diff) {
        # cat("===================================\n",
        #     "\r====== met", i, "(i) to met", j, "(j) =====\n",
        #     "\r===================================\n\n")
        for (co in co_names) {

          # Get the fluxes of production of met j
          flux_prod <- out_list$neighbs[[co]]$met[[mets[j]]]$prod_fluxes

          # Get the fluxes of consumption of met i
          flux_cons <- out_list$neighbs[[co]]$met[[mets[i]]]$cons_fluxes

          # Calculate the sums
          prod_j_sum <- sum(flux_prod[names(flux_prod) %in% edge_list[[co]]])
          cons_i_sum <- sum(flux_cons[names(flux_cons) %in% edge_list[[co]]])

          # Assign the sum for metabolite for each community
          out_list$flux_prod_j[[co]][i,j] <- round(prod_j_sum, 2)
          out_list$flux_cons_i[[co]][i,j] <- round(cons_i_sum, 2)

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

          # Calculate entropy
          prod_j_ent_filt <- 2^(-sum(prod_j_prob_filt * log2(prod_j_prob_filt)))
          cons_i_ent_filt <- 2^(-sum(cons_i_prob_filt * log2(cons_i_prob_filt)))

          # cat("-----", co, "All -----\n\rProbabilities Production j:\n")
          # print(round(prod_j_prob, 2))
          # cat("\nProbabilities Consumption i:\n")
          # print(round(cons_i_prob, 2))
          # cat(
          #   "\nEff n Flux Prod j: ", prod_j_ent,
          #   "\nEff n Flux Cons i: ", cons_i_ent, "\n\n"
          # )
          #
          # cat("-----", co, "Filtered -----\n\rProbabilities Production j:\n")
          # print(round(prod_j_prob_filt, 2))
          # cat("\nProbabilities Consumption i:\n")
          # print(round(cons_i_prob_filt, 2))
          # cat(
          #   "\nEff n Flux Prod j: ", prod_j_ent_filt,
          #   "\nEff n Flux Cons i: ", cons_i_ent_filt, "\n\n"
          # )
          out_list$flux_prod_j_eff[[co]][i,j] <- prod_j_ent_filt
          out_list$flux_cons_i_eff[[co]][i,j] <- cons_i_ent_filt
        }

        # Get the fluxes for production of j and consumption of i
        fluxes_p <-
          c(out_list$flux_prod_j[[1]][i,j], out_list$flux_prod_j[[2]][i,j])
        fluxes_c <-
          c(out_list$flux_cons_i[[1]][i,j], out_list$flux_cons_i[[2]][i,j]) * -1

        # Set the alignment as the minimum flux of the production of j
        out_list$alignment[i,j] <- min(fluxes_p)

        # Get the absolute differences
        f_p_abs_diff <- abs(fluxes_p[1] - fluxes_p[2])
        f_c_abs_diff <- abs(fluxes_c[1] - fluxes_c[2])

        # Get the max fluxes
        f_p_max <- max(fluxes_p)
        f_c_max <- max(fluxes_c)

        # Assign to respective matrix
        out_list$flux_prod_j_abs_diff[i,j] <- round(f_p_abs_diff, 2)
        out_list$flux_cons_i_abs_diff[i,j] <- round(f_c_abs_diff, 2)
        out_list$flux_prod_j_rel_diff[i,j] <- round(f_p_abs_diff / f_p_max, 2)
        out_list$flux_cons_i_rel_diff[i,j] <- round(f_c_abs_diff / f_c_max, 2)
      }
    }
  }
  return(out_list)
}

#' Functional Similarity of Microbial Communities (v2)
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

