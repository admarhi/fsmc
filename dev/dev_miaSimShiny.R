# Install miaSimShiny
pak::pkg_install("gaoyu19920914/miaSimShiny")
pak::pkg_install("microbiome/miaSim", dependencies = TRUE)
BiocManager::install("miaViz")
library(miaSimShiny)
miaSimShiny::run_app()
