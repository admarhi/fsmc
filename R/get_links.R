#' Get Links
#'
#' @param df Data frame with ...
#'
#' @return Tibble with links
#' @export
#'
#' @examples
#' #
get_links <- function(df) {

  # Get unique species
  species <- unique(df[,1][[1]])

  # Init output list
  sml <- list()

  # Iterate over all species
  for (s in species) {

    # Filter input for species rows
    sdf <- df[df[,1] == s, 2:3]

    # Init the hash
    m <- hash::hash()

    # Add fluxes to hash with the metabolite as key
    for (i in 1:nrow(sdf)) m[[as.character(sdf[i,1])]] <- as.numeric(sdf[i,2])

    # Add the flux to the output list
    sml[[s]] <- m }

  # Init output dataframe
  tb <- data.frame(
    from = character(), to = character(), met = character(), quant = numeric())

  # Iterate over species ( ### why again? )
  for (i in 1:length(species)) {

    # Get all species for which the flux is greater than 0
    k <- hash::keys(sml[[i]])[hash::values(sml[[i]]) > 0]

    # Iterate ove all species
    for (j in 1:length(species)) {

      # Skip same
      if (i == j) next

      # Get all species where the flux is smaller than 0
      l <- hash::values(
        sml[[j]])[hash::keys(sml[[j]]) %in% k & hash::values(sml[[j]]) < 0]

      # Skip if none exist
      if (length(l) == 0) next

      # Enter it to the output tibble
      for (x in 1:length(l)) {
        row <- nrow(tb) + 1
        tb[row,] <- c(names(sml)[i], names(sml)[j], names(l)[x], abs(l[x]))
      }
    }
  }
  return(tb)
}






