#' Functions to Change the orientation of MiCo Input Data
#' 
#' Wrapper function around tidyr's `pivot_longer()` function to facilitate
#' the easy transformation into the correct data format for MiCo construction. 
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
#' @examples
#' tb <- tibble::tribble(
#'   ~uptake, ~secretion, ~flux, ~species,
#'   "m1", "m2", 1, "s1",
#'   "m2", "m3", 1, "s2",
#'   "m3", "m4", 1, "s3",
#'   "m4", "m1", 1, "s4")
#' 
#' tb %>% pivotMiCo(
#'     species = "species",
#'     from = "uptake",
#'     to = "secretion",
#'     flux = "flux")
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