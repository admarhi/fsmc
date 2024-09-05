#' Functions to Change the orientation of MiCo Input Data
#' 
#' wrapper function around tidyr's `pivot_longer()` function to facilitate
#' the easy transformation between community data types. 
#' 
#' @param tb Tibble with data on a microbial community in 
#' long or short format to be 
#' @param species Column name of the species column
#' @param from the name of the column specifying the met consumed
#' @param to the name of the column specifying the met excreted
#' @param flux Column name of the flux column
#' 
#' @export
#' 
pivotMiCo <- function(tb, species, from, to, flux) {
  
  tb %>% 
    tidyr::pivot_longer(cols = c(from, to)) %>% 
    dplyr::rename(
      metabolite = "value",
      species = {{ species }},
      flux = {{ flux }}
    ) %>% 
    dplyr::mutate(
      flux = dplyr::if_else(.data$name == from, .data$flux * -1, .data$flux)
    ) %>% 
    dplyr::select("species", "metabolite", "flux")
}