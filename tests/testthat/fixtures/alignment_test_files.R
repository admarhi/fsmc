tb_set1 <- 
  tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m3", 1, "s2",
    "m3", "m4", 1, "s3",
    "m4", "m1", 1, "s4"
  ) %>% pivotMiCo( 
  species = "species", 
  from = "uptake", 
  to = "secretion", 
  flux = "flux")
readr::write_rds(tb_set1, "tests/testthat/fixtures/alig_test_set1.rds")



