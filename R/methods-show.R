#' Show Method for \code{MicrobiomeFunction} Object
#'
#' @param object An object of class \code{MicrobiomeFunction}
#' @export
setMethod("show", "MicrobiomeFunction", function(object) {
  stringr::str_glue(
    "{object@Name}: MicrobiomeFunction Object\n",
    "{ifelse(object@Weighted, 'Weighted', 'Unweighted')} ",
    "metabolic network with {length(object@Metabolites)} metabolites."
  ) %>% cat()
})


#' Show method for \code{MicrobiomeFunctionAlignment} Objects
#'
#' @param object a \code{MicrobiomeFunctionAlignment} object.
#' @export
setMethod("show", "MicrobiomeFunctionAlignment", function(object) {
  # alig_score <- "### ToDo ###"
  # max <- max(object@alignment$levels)
  # x <- object@alignment
  # x$levels <- x$levels_mat <- NULL
  # aligned_rxns <- x %>%
  #   purrr::keep(~.x$count == max) %>%
  #   names()

  # stringr::str_glue(
  #   "Microbial Community Alignment Object (MiCoAl)\n",
  #   "Max. alignment ({max}/{length(object@communities)} communities) in:\n",
  #   "{paste('-', aligned_rxns, collapse = '\n')}"
  # ) %>% cat()
})


