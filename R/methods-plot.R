setMethod("plot", "MicrobiomeFunction", function(x) {
  plot(x@Graphs[[1]])
})
