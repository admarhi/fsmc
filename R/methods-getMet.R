#' @rdname getMet
setMethod("getMet", "MiCo", function(object, unique = TRUE) {
  if (unique) return(unique(object@metabolites))
  object@metabolites
})


#' @rdname getMet
setMethod("getMet", "MiCoAl", function(object) {
  object@alignment$levels_mat %>% rownames()
})

