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
  species <- unique(df[,1][[1]])
  sml <- list()
  for (s in species) {
    sdf <- df[df[,1] == s, 2:3]
    m <- hash::hash()
    for (i in 1:nrow(sdf)) m[[as.character(sdf[i,1])]] <- as.numeric(sdf[i,2])
    sml[[s]] <- m }
  tb <- data.frame(from = character(), to = character(),
                   met = character(), quant = numeric())
  for (i in 1:length(species)) {
    k <- hash::keys(sml[[i]])[hash::values(sml[[i]]) > 0]
    for (j in 1:length(species)) {
      if (i == j) next
      l <- hash::values(sml[[j]])[hash::keys(sml[[j]]) %in% k & hash::values(sml[[j]]) < 0]
      if (length(l) == 0) next
      for (x in 1:length(l)) {
        row <- nrow(tb) + 1
        tb[row,] <- c(names(sml)[i], names(sml)[j], names(l)[x], abs(l[x]))
      }
    }
  }
  return(tb)
}






