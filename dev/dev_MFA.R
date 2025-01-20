# Analysis Alignment Score ---------------------------------------------------------
library(ggplot2)
ac_scores <- .scoreAlignment(ac_alig@alignment)
cit_scores <- .scoreAlignment(cit_alig@alignment)
f6p_scores <- .scoreAlignment(f6p_alig@alignment)

ac_scores %>%
  ggplot(aes(frac_comms, frac_rxns)) +
  geom_point() +
  theme_minimal()

cit_scores %>%
  ggplot(aes(frac_comms, 1/frac_rxns)) +
  geom_point() +
  theme_minimal()

f6p_scores %>%
  ggplot(aes(frac_comms, log(score))) +
  geom_point() +
  theme_minimal()

f6p_scores %>%
  dplyr::mutate(
    score2 = n_comms * n_edges
  ) %>% dplyr::arrange(dplyr::desc(score2))
  # dplyr::group_by(n_comms) %>%
  # dplyr::slice_max(.data$score, n = 1) %>%
  # dplyr::filter(frac_comms > 0.1) %>% dplyr::slice_max(score)
  ggplot(aes(frac_comms, score)) +
  geom_point() +
  geom_line() +
  # geom_smooth() +
  theme_minimal()




facilitation_score <- .scoreAlignment(facilitation_alig@alignment)
selection_score <- .scoreAlignment(selection_alig@alignment)

facilitation_score %>%
  ggplot(aes(frac_comms, score)) +
  geom_point() +
  theme_minimal()

selection_score %>%
  ggplot(aes(frac_comms, score)) +
  geom_point() +
  theme_minimal()

comp_scores <- .scoreAlignment(comp_alig@alignment)
coop_scores <- .scoreAlignment(coop_alig@alignment)

comp_scores %>%
  ggplot(aes(frac_comms, score)) +
  geom_point() +
  theme_minimal()

coop_scores %>%
  ggplot(aes(frac_comms, score)) +
  geom_point() +
  theme_minimal()

c1 <- newMiCo(ac_A1R12_1)
c2 <- newMiCo(ac_A1R12_2)
c3 <- newMiCo(cit_A1R12_1)
c4 <- newMiCo(cit_A1R12_2)
c5 <- newMiCo(ac_A3R04_1)

a <- newMiCoAl(c1, c2) #, c3, c4, c5)
b <- newMiCoAl(c3, c4)
c <- newMiCoAl(c1, c2, c5)
plotAlignmentHeatmap(a, frac = 0.2)
plotAlignmentHeatmap(b, frac = 0.2)
plotAlignmentHeatmap(c, frac = 0.2)

.scoreAlignment(a@alignment)
.scoreAlignment(b@alignment)
.scoreAlignment(c@alignment)

test$comms[2]
x <- miaFuncAlignmentResult()
x <- TreeSummarizedExperiment()
assays(x)[[1]] <- matrix(1:9, nrow = 3)

assays(tse)[[1]]



# Compare Alignments ------------------------------------------------------------------------------------
compareAlignments(
  ac_alig, cit_alig, f6p_alig,
  names = c("Acetate", "Citrate", "Fructose"),
  # min_frac = 0.2,
  smooth = TRUE)

compareAlignments(
  selection_alig, facilitation_alig,
  names = c("Selection", "Facilitation"),
  # min_frac = 0.05,
  # max_frac = 0.5,
  smooth = TRUE)

compareAlignments(
  coop_alig, comp_alig,
  names = c("cooperative", "competitive"),
  # min_frac = 0.2,
  smooth = F)

plotAlignmentHeatmap(coop_alig, 0.51)
plotAlignmentHeatmap(comp_alig, 0.31)

makeSynMiCo(name = 'test', n_species = 4, max_met = 4) %>% getCo()



tibble::tribble(
  ~species, ~metabolite, ~flux,
  "s1", "m1", -1,
  "s1", "m2", 1,
  "s2", "m2", -1,
  "s2", "m3", 1,
  "s3", "m3", -1,
  "s3", "m4", 1,
  "s4", "m4", -1,
  "s4", "m1", 1
)  %>% findEdges()

x <- tibble::tribble(
  ~uptake, ~secretion, ~fluxee, ~species,
  "m1", "m2", 1, "s1",
  "m2", "m3", 1, "s2",
  "m3", "m4", 1, "s3",
  "m4", "m1", 1, "s4"
)

y <- pivotMiCo(
  tb = x, 
  species = "species", 
  from = "uptake", 
  to = "secretion", 
  flux = "fluxee")

igraph::make_graph(y$metabolite) %>% plot()

biom_file_path <- system.file(
    "extdata", "Aggregated_humanization2.biom", package = "OMA")
sample_meta_file_path <- system.file(
    "extdata", "Mapping_file_ADHD_aggregated.csv", package = "OMA")
tree_file_path <- system.file(
    "extdata", "Data_humanization_phylo_aggregation.tre", package = "OMA")
readr::read_csv(sample_meta_file_path)
readr::read_delim(biom_file_path)
jsonlite::read_json("/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/OMA/extdata/Aggregated_humanization2.biom")

library(ape)

