#' @title Microbial Community Alignment Class
#'
#' @description A class to represent an alignment of multiple microbial
#' communities
#'
#' @slot communities A list of objects of type MicrobialCommunity.
#' @slot alignment A list containing the alignment data.
#'
#' @exportClass MiCoAl
#'
setClass(
  "MiCoAl",
  slots = c(
    communities = "list",
    alignment = "list"
  ),
  prototype = list(
    communities = list(),
    alignment = list()
  ),
  validity = function(object) {
    if (!all(sapply(object@communities, is, "MiCo"))) {
      return(
        "All elements must be MiCo objects")
    }
    TRUE
  }
)

#' Constructor for Microbial Community Alignment Objects
#'
#' @param ... MiCo objects to be aligned.
#' @param pairwise Logical indicating whether to perform pairwise alignment.
#'
#' @export
MiCoAl <- function(..., pairwise = FALSE) {

  # Make list of communities
  coms <- list(...)
  # Name the communities
  names(coms) <- sapply(coms, function(x) x@names)
  # Turn list into hash for quicker access
  coms <- hash::hash(coms)
  # Get all unique metabolites
  mets <- unique(unlist(lapply(coms, getMet)))

  # Init the alignment matrix
  alignment <- list()
  alignment$matrix <- matrix(
    data = 0,
    nrow = length(mets),
    ncol = length(mets),
    dimnames = list(mets, mets))
  alignment$hash <- hash::hash()

  # Iterate over all of the metabolites
  for (m1 in mets) {
    for (m2 in mets) {
      n <- 0
      for (co in hash::keys(coms)) {
        if (m1 %in% coms[[co]]@metabolites && m2 %in% coms[[co]]@metabolites) {
          n <- n + coms[[co]]@bin_matrix[m1, m2]
          alignment$hash[[paste0(m1, "_", m2)]] <- n
        }
      }
      alignment$matrix[m1, m2] <- n
    }
  }

  # Find the different levels of the alignment
  levels <-
    alignment$matrix %>%
    as.vector() %>%
    unique() %>%
    sort(decreasing = TRUE)



  # alignment$binary_matrix <-
  #   Reduce(`*`, lapply(communities, function(obj) slot(obj, "bin_matrix")))
  ### This would only work if the matrices have exactly the same size.
  ### Need to iterate instead over the matrices and multiply the values for
  ### the corresponding edges together.

  ### Implement the overall score in the next step!

  methods::new(
    "MiCoAl",
    communities = hash::as.list.hash(coms),
    alignment = alignment)
}
