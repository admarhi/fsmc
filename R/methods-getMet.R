#' @rdname getMet
setMethod("getMet", "MicrobiomeFunction", function(object) {
  object@Metabolites
})


#' @rdname getMet
setMethod("getMet", "MicrobiomeFunctionAlignment", function(object) {
  ### ToDo
})
