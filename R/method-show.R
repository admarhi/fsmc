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
  stringr::str_glue(
    "Microbial Community Alignment Object (MiCoAl)\n",
    "Alignment of {length(object@communities)} communities with an overall ",
    "score of {alig_score}."
  ) %>% cat()
})


