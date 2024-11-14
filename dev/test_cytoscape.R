BiocManager::install("RCy3")

RCy3::cytoscapePing ()
RCy3::cytoscapeVersionInfo ()


nodes <- data.frame(
  id = unique(c(t$from, t$to))#,
  #group = c("A", "A", "B", "B"),
  # categorical strings
  #score = as.integer(c(20, 10, 15, 5)),
  # integers
  #stringsAsFactors = FALSE
)

edges <- data.frame(
  source = t$from, #c("node 0", "node 0", "node 0", "node 2"),
  target = t$to, #c("node 1", "node 2", "node 3", "node 3"),
  # interaction = c("inhibits", "interacts", "activates", "interacts"),
  # optional
  weight = t$quant, #c(5.1, 3.0, 5.2, 9.9),
  # numeric
  stringsAsFactors = FALSE
)

RCy3::createNetworkFromDataFrames(
  nodes, edges, title = "my first network",
  collection = "DataFrame Example")


