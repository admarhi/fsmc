#' Show method for Microbial Community objects
#'
#' @param object An object of class MiCo
#' @export
setMethod("show", "MiCo", function(object) {
  stringr::str_glue(
    "{object@name}: MiCo (MicrobialCommunity) Object\n",
    " - Unique microorganisms (MO): {length(unique(object@species))}\n",
    " - Unique metabolites (met): {length(unique(object@metabolites))}"
  ) %>% cat()
})


#' Show method for Microbial Community Alignment Objects
#'
#' @param object An object of class MiCoAl
#' @export
setMethod("show", "MiCoAl", function(object) {
  alig_score <- "### ToDo ###"
  max <- max(object@alignment$levels)
  x <- object@alignment
  x$levels <- x$levels_mat <- NULL
  aligned_rxns <- x %>%
    purrr::keep(~.x$count == max) %>%
    names()

  stringr::str_glue(
    "Microbial Community Alignment Object (MiCoAl)\n",
    "Max. alignment ({max}/{length(object@communities)} communities) in:\n",
    "{paste('-', aligned_rxns, collapse = '\n')}"
  ) %>% cat()
})


