#' Show Method for \code{MicrobiomeFunction} Object
#'
#' @param object An object of class \code{MicrobiomeFunction}
#' @export
setMethod("show", "MicrobiomeFunction", function(object) {
  stringr::str_glue(
    "\n{object@Name}: MicrobiomeFunction Object\n",
    "{ifelse(object@Weighted, 'Weighted', 'Unweighted')} ",
    "metabolic network with {length(object@Metabolites)} metabolites.\n\n"
  ) %>%
    cat()
})

#' Show method for \code{MicrobiomeFunctionAlignment} Objects
#'
#' @param object a \code{MicrobiomeFunctionAlignment} object.
#' @export
setMethod("show", "MicrobiomeFunctionAlignment", function(object) {
  # alig_score <- "### ToDo ###"
  x <- hash::as.list.hash(object@Alignment)
  max <- unique(max(x$levels))
  x$levels <- x$levels_mat <- NULL
  aligned_rxns <- x %>%
    purrr::keep(~.x$count == max) %>%
    names()

  if (length(aligned_rxns) > 5) aligned_rxns <- c(aligned_rxns[1:5], "...")

  max_score <- object@Score %>%
    dplyr::slice_max(.data$score)

  stringr::str_glue(
    "Functional Alignment of {length(object@Communities)} Microbiomes\n\n",
    "- Max. identity {round(max_score$score[[1]], 4)}",
    " for {nrow(max_score)} set(s) of communities\n",
    "- Depth at max. identity: {paste(round(max_score$depth, 4), collapse = ', ')}\n",
    "- Breadth at max. identity: {paste(round(max_score$breadth, 4), collapse = ', ')}\n\n"
  ) %>%
    cat()
})
