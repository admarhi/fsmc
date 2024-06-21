setGeneric("plotAlignment", function(object, ...) {
  standardGeneric("plotAlignment")
})

setMethod("plotAlignment", "MiCoAl", function(object, ...) {
  g <- igraph::graph_from_adjacency_matrix(
    object@alignment$matrix,
    mode = "directed",
    weighted = TRUE)

  igraph::E(g)$width <- (igraph::E(g)$weight / max(igraph::E(g)$weight) * 4)
  igraph::E(g)$arrow.size <- (igraph::E(g)$weight / max(igraph::E(g)$weight))

  colors <- rev(c("darkred", "red", "orange", "pink"))


  igraph::E(g)$color <- colors[igraph::E(g)$weight]


  plot(
    g,
    layout = igraph::layout_with_fr(g),

    # edge.width = igraph::E(g)$width,
    # edge.color = igraph::E(g)$color,
    edge.curved = 0.5,
    vertex.label.color = "black",
    vertex.color = "lightblue",
    main = "Graph with Custom Edge Widths and Colors")

})
