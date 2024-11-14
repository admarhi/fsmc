
msR_4clust <-  dplyr::select(msR_clean, 3:6)
clusters <- kmeans(msR_4clust, 3)

fpc::calinhara(msR_4clust, clusters$cluster)





g <- igraph::graph_from_adjacency_matrix(
  adjmatrix = t1,
  weighted = TRUE)

plot(
  g,
  vertex.color = "green",
  vertex.size = 10,
  edge.arrow.size = 0.5,
  edge.curved = 0.2
)

igraph::cluster_edge_betweenness(g)



g2 <- igraph::graph_from_adjacency_matrix(
  adjmatrix = t3,
  weighted = TRUE)

plot(
  g2,
  vertex.color = "green",
  vertex.size = 10,
  edge.arrow.size = 0.5,
  edge.curved = 0.2
)

summary(g)
igraph::cluster_edge_betweenness(g2, weights = NA)

tree <- igraph::make_tree(20, 2)
plot(g, edge.arrow.size = 0.1)
