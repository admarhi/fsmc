#' @param object An object of class MiCo
#' @describeIn getCo Get the Community
#' @return A tibble with the community data.
#' @export
setMethod("getCo", "MiCo", function(object) {
  tibble::tibble(
    species = object@species,
    metabolites = object@metabolites,
    fluxes = object@fluxes
  )
})

#' @param object An object of class MiCoAl
#' @describeIn getCo Get the Community
#' @return A list with the community data.
#' @export
setMethod("getCo", "MiCoAl", function(object) {
  object@communities %>% purrr::map(getCo)
})

