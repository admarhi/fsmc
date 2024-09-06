# Set 1
tibble::tribble(
  ~uptake, ~secretion, ~flux, ~species,
  "m1", "m2", 1, "s1",
  "m2", "m3", 1, "s2",
  "m3", "m4", 1, "s3",
  "m4", "m1", 1, "s4"
) %>%
  pivotMiCo(
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  ) %>%
  readr::write_rds("tests/testthat/fixtures/aligment_test_set1.rds")

# Set2
list(
  c1 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m3", 1, "s2",
    "m3", "m4", 1, "s3",
    "m4", "m1", 1, "s4",
    "m1", "m4", 1, "s5",
  ),
  c2 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m3", 1, "s2",
    "m3", "m4", 1, "s3",
    "m4", "m1", 1, "s4",
    "m4", "m3", 1, "s5",
  ),
  c3 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m3", 1, "s2",
    "m3", "m4", 1, "s3",
    "m4", "m1", 1, "s4",
    "m3", "m2", 1, "s5",
  ),
  c4 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m3", 1, "s2",
    "m3", "m4", 1, "s3",
    "m4", "m1", 1, "s4",
    "m2", "m1", 1, "s5",
  )
) %>%
  purrr::map(pivotMiCo,
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  ) %>%
  readr::write_rds("tests/testthat/fixtures/aligment_test_set2.rds")


# Set3
list(
  c1 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m4", 1, "s2",
    "m4", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m2", "m3", 1, "s5",
  ),
  c2 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m4", 1, "s2",
    "m4", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m3", "m2", 1, "s5",
  ),
  c3 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m3", 1, "s1",
    "m3", "m4", 1, "s2",
    "m4", "m2", 1, "s3",
    "m2", "m1", 1, "s4",
    "m1", "m4", 1, "s5",
  ),
  c4 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m3", 1, "s1",
    "m3", "m4", 1, "s2",
    "m4", "m2", 1, "s3",
    "m2", "m1", 1, "s4",
    "m4", "m1", 1, "s5",
  )
) %>%
  purrr::map(pivotMiCo,
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  ) %>%
  readr::write_rds("tests/testthat/fixtures/aligment_test_set3.rds")

# Set 4
list(
  c1 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m4", 1, "s2",
    "m3", "m1", 1, "s4",
  ),
  c2 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m4", 1, "s2",
    "m4", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
  ),
  c3 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m4", 1, "s2",
    "m4", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m1", "m4", 1, "s5",
  ),
  c4 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m2", "m4", 1, "s2",
    "m4", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m3", "m2", 1, "s4",
    "m1", "m4", 1, "s5",
  )
) %>%
  purrr::map(pivotMiCo,
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  ) %>%
  readr::write_rds("tests/testthat/fixtures/aligment_test_set4.rds")

# Set 5
list(
  c1 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m3", "m4", 1, "s2",
    "m3", "m1", 1, "s3",
  ),
  c2 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m1", "m3", 1, "s2",
    "m2", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m3", "m2", 1, "s5",
    "m3", "m4", 1, "s6",
  ),
  c3 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m1", "m3", 1, "s2",
    "m2", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m3", "m2", 1, "s5",
    "m3", "m4", 1, "s6",
    "m4", "m2", 1, "s7",
  ),
  c4 = tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m1", "m3", 1, "s2",
    "m2", "m3", 1, "s3",
    "m3", "m1", 1, "s4",
    "m3", "m2", 1, "s5",
    "m3", "m4", 1, "s6",
    "m4", "m2", 1, "s7",
  )
) %>%
  purrr::map(pivotMiCo,
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  ) %>%
  readr::write_rds("tests/testthat/fixtures/aligment_test_set5.rds")
