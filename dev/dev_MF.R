test0 <- tibble::tribble(
  ~uptake,
  ~secretion,
  ~flux,
  ~species,
  "m1",
  "m2",
  1,
  "s1",
  "m1",
  "m3",
  1,
  "s1",
  "m1",
  "m3",
  1,
  "s2",
  "m1",
  "m4",
  1,
  "s2",
  "m2",
  "m3",
  1,
  "s2",
  "m2",
  "m4",
  1,
  "s2",
  "m3",
  "m2",
  1,
  "s3",
  "m3",
  "m4",
  1,
  "s3"
) %>%
  pivotMF(
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  )

# test0 <- MicrobiomeFunction(test0, "test00")
# plot(test0)

test1 <- misosoup24$ac_A1R12_1 %>%
  dplyr::rename(flux = fluxes, met = metabolites)

test2 <- misosoup24$cit_A1R12_1 %>%
  dplyr::rename(flux = fluxes, met = metabolites)

coocc <- readr::read_csv("../fsmc-data/cooccurrence/cooperative.csv") |>
  dplyr::select(-fluxes, community, species, met = metabolites, flux = smetana)

co0 <- coocc |>
  dplyr::filter(community == "coop_0") |>
  dplyr::select(-community) |>
  MicrobiomeFunction("coop_0")

co1 <- coocc |>
  dplyr::filter(community == "coop_1") |>
  dplyr::select(-community) |>
  MicrobiomeFunction("coop_1")

co2 <- coocc |>
  dplyr::filter(community == "coop_2") |>
  dplyr::select(-community) |>
  MicrobiomeFunction("coop_2")

co20 <- coocc |>
  dplyr::filter(community == "coop_20") |>
  dplyr::select(-community) |>
  MicrobiomeFunction("coop_20")

x <- MicrobiomeFunction(test1, "test")
y <- MicrobiomeFunction(test2, "test2")

mb_test <- coocc |>
  dplyr::filter(community == "coop_20") |>
  dplyr::select(-community)

microbenchmark::microbenchmark(
  MicrobiomeFunction(mb_test, "test"),
  unit = "seconds"
)

### Result before sparse matrix implementation
# Unit: seconds
#                                 expr       min        lq      mean   median        uq     max neval
#  MicrobiomeFunction(mb_test, "test") 0.1634916 0.1835104 0.4195387 0.194103 0.2153653 1.61403   100

# Unit: seconds
#                                 expr       min        lq      mean    median        uq      max neval
#  MicrobiomeFunction(mb_test, "test") 0.1692718 0.1843308 0.2148147 0.1928256 0.2043011 1.572195   100

Matrix::sparseMatrix(
  c(2, 2, 3, 8),
  c(6, 9, 5, 12),
  x = c(1, 2, 3, 4),
  dims = c(15, 16)
)

mets <- tibble::tibble(
  met = sort(unique(mb_test$met)),
  index = seq_len(length(unique(mb_test$met)))
)

species <- tibble::tibble(
  species = sort(unique(mb_test$species)),
  species_code = paste0("s", seq_len(length(unique(mb_test$species))))
)

tb <- mb_test |>
  # Ensure that no zero values are included
  dplyr::filter(.data$flux != 0) |>
  # recode the species names
  dplyr::left_join(species, by = "species") |>
  dplyr::select(-"species", species = "species_code")

### Check if any rows have flux == 0
cons <- tb |>
  dplyr::filter(.data$flux < 0) |>
  dplyr::mutate(flux = .data$flux * -1) |>
  dplyr::reframe(flux = sum(.data$flux), .by = c("species", "met")) |>
  dplyr::rename(consumed = "met")

prod <- tb |>
  dplyr::filter(.data$flux > 0) |>
  dplyr::reframe(flux = sum(.data$flux), .by = c("species", "met")) |>
  dplyr::rename(produced = "met")

out <- cons |>
  ### Need to double check the logic with alberto
  dplyr::inner_join(
    prod,
    by = "species",
    suffix = c("_cons", "_prod"),
    relationship = "many-to-many"
  ) |>
  tidyr::nest(data = c("species", "flux_cons", "flux_prod")) |>
  dplyr::mutate(
    n_species = purrr::map_dbl(.data$data, \(x) nrow(x)),

    # Get the sum of the fluxes
    c_sum = purrr::map_dbl(.data$data, \(x) sum(x$flux_cons)),
    p_sum = purrr::map_dbl(.data$data, \(x) sum(x$flux_prod)),

    c_prob = purrr::map(.data$data, \(x) x$flux_cons / sum(x$flux_cons)),
    p_prob = purrr::map(.data$data, \(x) x$flux_prod / sum(x$flux_prod)),

    c_eff = purrr::map_dbl(.data$c_prob, \(x) round(2**(-sum(x * log2(x))), 2)),
    p_eff = purrr::map_dbl(.data$p_prob, \(x) round(2**(-sum(x * log2(x))), 2))
  ) |>
  # Replace with the indeces for the metabolites
  dplyr::left_join(mets, by = c(consumed = "met")) |>
  dplyr::rename(c_ind = "index") |>
  dplyr::left_join(mets, by = c(produced = "met")) |>
  dplyr::rename(p_ind = "index")

out
library(Matrix)

Binary <- sparseMatrix(out$c_ind, out$p_ind, x = 1)
nEdges <- sparseMatrix(out$c_ind, out$p_ind, x = out$n_species)
Consumption <- sparseMatrix(out$c_ind, out$p_ind, x = out$c_sum)
Production <- sparseMatrix(out$c_ind, out$p_ind, x = out$p_sum)
EffectiveConsumption <- sparseMatrix(out$c_ind, out$p_ind, x = out$c_eff)
EffectiveProduction <- sparseMatrix(out$c_ind, out$p_ind, x = out$p_eff)

igraph::graph_from_adjacency_matrix(
  adjmatrix = Binary,
  mode = "directed"
)
plot(x@Graphs[[1]])
plot(y@Graphs[[1]])
plot(x)
plot(y)

o <- MicrobiomeFunctionAlignment(x, y, name = "test_alignment")
assay(o)
plotAlignmentNetwork(o, 0.8)

x@Metabolites
getMet(x)
getMet(y, x)
getMet(o)
x@Metabolites

x@assays
MultiAssayExperiment::assays(x)$Binary %>% class()

cit_a <- MicrobiomeFunctionAlignment(cit[1:3], name = "cit_a")
ac_a <- MicrobiomeFunctionAlignment(ac[1:3], name = "ac_a")

test2 <- tibble::tribble(
  ~uptake,
  ~secretion,
  ~flux,
  ~species,
  "m4",
  "m2",
  1.6,
  "s1",
  "m1",
  "m3",
  1,
  "s1",
  "m1",
  "m3",
  1.2,
  "s2",
  "m1",
  "m4",
  0.4,
  "s2",
  "m2",
  "m3",
  1,
  "s2",
  "m2",
  "m4",
  1,
  "s2",
  "m3",
  "m2",
  2.6,
  "s3",
  "m3",
  "m4",
  1,
  "s3"
)

mets <- sort(unique(c(test2$uptake, test2$secretion)))
code_tibble <- tibble::tibble(
  met = unique(mets),
  code = seq(length(mets))
)


test3 <- test2 %>%
  left_join(code_tibble, by = join_by(uptake == met)) %>%
  rename(code_u = "code") %>%
  left_join(code_tibble, by = join_by(secretion == met)) %>%
  rename(code_s = "code")

### Need a check that all edges are unique, i.e., the combination of
### uptake, secretion and species.

mat <- matrix(rep(0, length(mets)**2), nrow = length(mets))
for (i in seq(nrow(test3))) {
  row <- test3$code_u[i]
  col <- test3$code_s[i]

  mat[row, col] <- mat[row, col] + 1
}


microbenchmark::microbenchmark(
  MicrobiomeFunction(test0, "test")
)

# Results 19.01.2025
# MicrobiomeFunction(test0, "test") 103.6132 111.7186 118.8462 115.9255 123.3139 180.822   100

coop <-
  readr::read_csv("dev/archive/TFM/cooccurrence_clean/cooperative_raw.csv") |>
  dplyr::rename(
    to = "receiver",
    from = "donor"
  ) |>
  dplyr::mutate(
    compound = stringr::str_remove_all(.data$compound, "^M_|_e$"),
    community = stringr::str_replace(
      .data$community,
      "rq_subsample",
      "coop"
    )
  )


coop1 <- coop |>
  filter(community == "coop_1") |>
  select(to, from, compound, smetana) |>
  tidyr::pivot_longer(cols = c(to, from)) |>
  rename(
    met = compound,
    flux = smetana,
    species = value
  ) |>
  mutate(flux = if_else(name == "to", flux * -1, flux))

old_coop1 <- findEdges(coop1)

microbenchmark::microbenchmark(
  findEdges(coop1)
)

# findEdges(coop1) 3.456772 3.793705 4.340301 4.027832 4.656197 7.81123   100

oldMF_coop <- MicrobiomeFunction(coop1)

microbenchmark::microbenchmark(
  MicrobiomeFunction(coop1)
)

# MicrobiomeFunction(coop1) 134.38 143.0807 153.4163 147.8342 158.5333 233.8846   100
