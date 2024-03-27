# MiCo.align <- function(
#     co_list,
#     edges_n = TRUE,
#     edges_js = TRUE,
#     fluxes = TRUE,
#     fluxes_diff = TRUE) {
#
#   # Create output list
#   out_list <- list()
#
#   # Get number of communities
#   n_co <- length(co_list)
#
#   # Print warning about flux score
#   if (n_co > 2) message("\nFluxes only available for first two comms!\n")
#
#   # Get names
#   co_names <- names(co_list)
#
#   # Save communities in out_list
#   out_list$comms <- co_list
#
#   # Iterate over communities
#   for (co in co_names) {
#
#     # Get neighbors of all communities
#     out_list$neighbs[[co]] <- get_neighbs(out_list$comms[[co]], silent = TRUE)
#
#     # Get intersecting metabolites
#     if (co == co_names[1]) {
#       mets <- unique(out_list$comms[[1]]$met)
#     } else {
#       mets <- sort(intersect(mets, unique(out_list$comms[[co]]$met)))
#     }
#   }
#
#   # Throw error if no common metabolites
#   if (length(mets) == 0) stop("No common metabolites between communities.")
#
#   ### Make nice output about which metabolites will be aligned
#
#   # Add mets to output
#   out_list$mets <- mets
#
#   # Create raw 0 matrix of dimension length(mets) * length(mets)
#   raw_mat <- matrix(rep(0, length(mets)^2), nrow = length(mets))
#
#   # Create NA mat ### could do this as raw and assign 0 in bin_mat creation
#   na_mat <- matrix(rep(NA, length(mets)^2), nrow = length(mets))
#
#   # Iterate over all communities
#   for (co in co_names) {
#
#     # Set 0 matrix
#     out_list$bin_matrices[[co]] <- raw_mat
#
#     # Iterate over input mets
#     for (i in 1:length(mets)) {
#       # Get consumers met i
#       consu_i <- out_list$neighbs[[co]]$met[[mets[i]]]$consumers
#
#       # Iterate over output mets
#       for (j in 1:length(mets)) {
#
#         # Set NA if on diagonal
#         if (i == j) { out_list$bin_matrices[[co]][i,j] <- NA; next }
#
#         # Get producers of met j
#         prods_j <- out_list$neighbs[[co]]$met[[mets[j]]]$producers
#
#         # Get the intersection between prods_i and cons_i (edges)
#         edges <- Reduce(intersect, list(prods_j, consu_i))
#
#         # Skip if no edges
#         if (length(edges) == 0) next
#
#         # Set binary matrix to 1
#         out_list$bin_matrices[[co]][i,j] <- 1
#
#         # Save the edges to out_list
#         out_list$edges[[co]][[paste0(mets[i], "_to_", mets[j])]] <- edges
#       }
#     }
#   }
#
#   # Calculate the bin mat alignment
#   bin_mat <- Reduce(`*`, out_list$bin_matrices)
#   out_list$bin_mat <- bin_mat
#
#   # Stop if nothing could be aligned
#   if (all(bin_mat == 0)) {
#     cat("No alignment found!")
#     return(out_list)
#   }
#
#   # Check if additional required
#   if (all(!edges_js, !fluxes_diff, !edges_n, !fluxes)) return(out_list)
#
#   # Init matrices depending on options
#   if (edges_js) out_list$js_mat <- bin_mat
#   if (fluxes_diff) {
#     out_list$fluxes$flux_abs_mat <- na_mat
#     out_list$fluxes$flux_rel_mat <- na_mat
#     out_list$fluxes$ent_abs_mat <- na_mat
#     out_list$fluxes$ent_rel_mat <- na_mat
#   }
#   for (co in co_names) {
#     if (edges_n) out_list$edges_n[[co]] <- bin_mat
#     if (fluxes) {
#       out_list$fluxes$total[[co]] <- na_mat
#       # out_list$fluxes$min[[co]] <- na_mat
#       # out_list$fluxes$max[[co]] <- na_mat
#       out_list$fluxes$effective[[co]] <- na_mat
#     }
#   }
#
#   # Calculate required values
#   for (i in seq(nrow(bin_mat))) {
#     for (j in seq(ncol(bin_mat))) {
#
#       if (is.na(bin_mat[i,j]) | bin_mat[i,j] == 0) next
#
#       # Make the edge name
#       edge <- paste0(mets[i], "_to_", mets[j])
#
#       # Get the edges
#       edge_list <- purrr::map_depth(out_list$edges, .depth = 1, edge)
#
#       # Total number of edges
#       if (edges_n) {
#         for (co in co_names) {
#           n_edge <- length(out_list$edges[[co]][[edge]])
#           out_list$edges_n[[co]][i,j] <- n_edge
#         }
#       }
#
#       ##### Jaccard Score #####
#       if (edges_js) {
#
#         # Get the intersection
#         edge_intersect <- Reduce(intersect, edge_list)
#
#         # Get the union
#         edge_union <- unname(unlist(edge_list))
#
#         # Calculate the Jaccard Score
#         jaccard <- length(edge_intersect) / length(edge_union)
#
#         # Set to 0 if NaN
#         if (is.nan(jaccard)) jaccard <- 0
#
#         # Write to Jaccard matrix
#         out_list$js_mat[i,j] <- out_list$js_mat[i,j] * jaccard
#
#         # Round the Jaccard matrix
#         out_list$js_mat <- round(out_list$js_mat, 2)
#       }
#
#       ##### Fluxes #####
#       if (fluxes) {
#         for (co in co_names) {
#
#           # Get the production flux of met j
#           all_p_flux_j <- out_list$neighbs[[co]]$met[[mets[j]]]$prod_fluxes
#
#           # Filter for fluxes of the current edge
#           p_flux_j <- all_p_flux_j[names(all_p_flux_j) %in% edge_list[[co]]]
#
#           # Take the sum of the total edge
#           sum_p_flux_j <- sum(p_flux_j)
#
#           # Assign the sum for metabolite for each community
#           out_list$fluxes$total[[co]][i,j] <- sum_p_flux_j
#
#           # Assign the minimum and maximum for each community
#           # out_list$fluxes$min[[co]][i,j] <- min(p_flux_j)
#           # out_list$fluxes$max[[co]][i,j] <- max(p_flux_j)
#
#           # Calculate the probability of each flux value
#           probabilities <- p_flux_j / sum_p_flux_j
#
#           # Calculate the entropy of the flux distribution
#           entropy <- -sum(probabilities * log2(probabilities))
#
#           # Assign the entropy to the output
#           out_list$fluxes$effective[[co]][i,j] <- entropy
#         }
#       }
#
#       ##### Flux Differences #####
#       if (fluxes_diff) {
#
#         # Init vectors
#         sums <- vector()
#         ents <- vector()
#
#         for (co in co_names) {
#
#           # Get the flux sums for met j
#           sums <- c(sums, out_list$fluxes$total[[co]][i,j])
#
#           # Get the entropy for met j
#           ents <- c(ents, out_list$fluxes$effective[[co]][i,j])
#         }
#
#         # Get the absolute difference
#         flux_abs_diff <- abs(sums[1] - sums[2])
#
#         # Get the max flux
#         flux_max <- max(sums)
#
#         # Assign to respective matrix
#         out_list$fluxes$flux_abs_mat[i,j] <- round(flux_abs_diff, 2)
#         out_list$fluxes$flux_rel_mat[i,j] <- round(flux_abs_diff / flux_max, 2)
#       }
#     }
#   }
#   return(out_list)
# }
#
