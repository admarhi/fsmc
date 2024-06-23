#### MisoSoup ####
ac_A1R12_1 <-
  read.csv("inst/extdata/misosoup/ac_A1R12_1.csv") %>%
  tibble::as_tibble()
usethis::use_data(ac_A1R12_1, overwrite = TRUE)

cit_A1R12_1 <-
  read.csv("inst/extdata/misosoup/cit_A1R12_1.csv") %>%
  tibble::as_tibble()
usethis::use_data(cit_A1R12_1, overwrite = TRUE)
