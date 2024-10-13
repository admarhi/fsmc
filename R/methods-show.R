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
  x <- hash::as.list.hash(object@Alignment)
  max <- max(x$levels)
  x$levels <- x$levels_mat <- NULL
  aligned_rxns <- x %>%
    purrr::keep(~.x$count == max) %>%
    names()

  if (length(aligned_rxns) > 5) aligned_rxns <- c(aligned_rxns[1:5], "...")
  
  stringr::str_glue(
    "Functional Alignment of {length(object@Communities)} Microbiomes\n\n",
    "- Identity: {round(object@Score$score, 4)}\n",
    "- Depth: {round(object@Score$depth, 4)}\n",
    "- Breadth: {round(object@Score$breadth, 4)}\n\n",
    "Max. alignment ({max}/{length(object@Communities)} communities) in:\n",
    "{paste('-', aligned_rxns, collapse = '\n')}"
  ) %>% cat()
})


