#' Get the Community
#'
#' Returns the community of a single MiCo object in a tibble format or a list
#' of communities in tibble format for MiCoAl objects.
#'
#' @export
#'
#' @examples
#' c1 <- newMiCo(ac_A1R12_1)
#' c2 <- newMiCo(ac_A1R12_2)
#' a <- newMiCoAl(c1, c2)
#'
#' getCo(c1)
#' getCo(a)
setGeneric("getCo", function(object) {
  standardGeneric("getCo")
})


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

