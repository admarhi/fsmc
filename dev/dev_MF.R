test0 <- tibble::tribble(
  ~uptake, ~secretion, ~flux, ~species,
  "m1", "m2", 1, "s1",
  "m1", "m3", 1, "s1",
  "m1", "m3", 1, "s2",
  "m1", "m4", 1, "s2",
  "m2", "m3", 1, "s2",
  "m2", "m4", 1, "s2",
  "m3", "m2", 1, "s3",
  "m3", "m4", 1, "s3"
) %>%
  pivotMiCo(
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  )

test1 <- ac_A1R12_1 %>% 
  dplyr::rename(
    flux = fluxes,
    met = metabolites
  )

test2 <- cit_A1R12_1%>% 
  dplyr::rename(
    flux = fluxes,
    met = metabolites
  )

x <- MicrobiomeFunction(test1, "test")
y <- MicrobiomeFunction(test2, "test2")

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
ac_a <-  MicrobiomeFunctionAlignment(ac[1:3], name = "ac_a")

test2 <- tibble::tribble(
  ~uptake, ~secretion, ~flux, ~species,
  "m4", "m2", 1.6, "s1",
  "m1", "m3", 1, "s1",
  "m1", "m3", 1.2, "s2",
  "m1", "m4", 0.4, "s2",
  "m2", "m3", 1, "s2",
  "m2", "m4", 1, "s2",
  "m3", "m2", 2.6, "s3",
  "m3", "m4", 1, "s3"
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
      .data$community, "rq_subsample", "coop")
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