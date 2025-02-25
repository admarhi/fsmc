#' MiCoAl Network Plot
#'
#' @param object An object of class MiCoAl.
#' @param frac Minimum relative weight of maximum weight to plot.
#'
#' @return An igraph network
#' @export
plotAlignmentNetwork <- function(object, frac) {
  # Filter the adjacency matrix for desired levels for visualization.
  levels_mat <- object@Alignment$levels_mat
  max_weight <- length(object@Communities)
  min_weight <- max_weight * frac

  levels_mat[levels_mat < min_weight] <- 0

  g <- igraph::graph_from_adjacency_matrix(
    levels_mat,
    mode = "directed",
    weighted = TRUE
  )

  # Identify isolated vertices (those with degree 0)
  isolated_vertices <- igraph::V(g)[igraph::degree(g) == 0]

  # Remove isolated vertices
  g <- igraph::delete_vertices(g, isolated_vertices)

  rel <- (igraph::E(g)$weight / max_weight * 4)
  igraph::E(g)$width <- rel
  igraph::E(g)$arrow.size <- 0.8

  plot(
    g,
    layout = igraph::layout_with_fr(g),
    edge.curved = 0.5,
    vertex.label.color = "black",
    vertex.color = "lightblue",
    main = glue::glue("Microbial Community Alignment")
  )

  invisible(g)
}
