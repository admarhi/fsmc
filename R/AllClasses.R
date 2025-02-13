#' @export
#' @import methods
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
newMicrobiomeFunction <- setClass(
  Class = "MicrobiomeFunction",
  contains = "TreeSummarizedExperiment",
  slots = list(
    Name = "character",
    Edges = "list",
    Weighted = "logical",
    InputData = "data.frame",
    Metabolites = "character",
    Graphs = "list"
  )
)

#' @export
#' @import methods
newMicrobiomeFunctionSet <- setClass(
  Class = "MicrobiomeFunctionSet",
  ### don't think I need to import TSE here
  # contains = "TreeSummarizedExperiment",
  slots = list(
    Name = "character",
    Communities = "list",
    Description = "character"
  ),
  prototype = list(
    Name = NA_character_,
    Communities = list(),
    Description = NA_character_
  )
)

#' @export
#' @import methods
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
newMicrobiomeFunctionAlignment <- setClass(
  Class = "MicrobiomeFunctionAlignment",
  contains = "TreeSummarizedExperiment",
  slots = list(
    Name = "character",
    Alignment = "hash",
    Communities = "list",
    Score = "data.frame"
  )
)
