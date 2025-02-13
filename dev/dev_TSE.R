c1 <- ac_A1R12_1 %>% 
  tidyr::pivot_wider(
    id_cols = species, 
    names_from = metabolites, 
    values_from = fluxes)

c2 <-
  readr::read_csv("dev/archive/TFM/cooccurrence_clean/competitive_old.csv") %>% 
  dplyr::filter(community == "comp_0") %>% 
  tidyr::pivot_wider(
    id_cols = species,
    names_from = compound,
    values_from = flux
  )


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


c2 %>% 
  dplyr::group_by(to, compound) %>% 
  dplyr::summarise(
    n = dplyr::n(),
    tot_flux = sum(fluxes)
  )


coop %>% 
  dplyr::filter(community == "coop_0") %>% 
  dplyr::group_by(community, from, compound) %>%
  dplyr::summarise(n = dplyr::n())



# Evaluate how to store data in TSE objects ----------------------------------------
m1 <- matrix(c(0,1,0,0,0,1,1,0,0), nrow = 3, byrow = TRUE)
m2 <- matrix(c(0,0,0,0,0,1,1,0,0), nrow = 3, byrow = TRUE)
m1 * m2

# Basic mia functionality -----------------------------------------------------------
data("GlobalPatterns", package="mia")
tse <- GlobalPatterns

TreeSummarizedExperiment::colTree(tse)
TreeSummarizedExperiment::rowTree(tse)
TreeSummarizedExperiment::rowLinks(tse)
metadata(tse)
