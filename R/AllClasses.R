#' @export
#' @import methods
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
.MicrobiomeFunction <- setClass(
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
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
.MicrobiomeFunctionAlignment <- setClass(
  Class = "MicrobiomeFunctionAlignment", 
  contains = "TreeSummarizedExperiment",
  slots = list(
    Name = "character",
    Alignment = "hash",
    Communities = "list",
    Score = "data.frame"
  )
)