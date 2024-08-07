#### MisoSoup ####

### Could delete all but three bc they're doubled with the list
ac_A1R12_1 <-
  read.csv("inst/extdata/misosoup/ac_A1R12_1.csv") %>%
  tibble::as_tibble()
usethis::use_data(ac_A1R12_1, overwrite = TRUE)

ac_A1R12_2 <-
  read.csv("inst/extdata/misosoup/ac_A1R12_2.csv") %>%
  tibble::as_tibble()
usethis::use_data(ac_A1R12_2, overwrite = TRUE)

cit_A1R12_1 <-
  read.csv("inst/extdata/misosoup/cit_A1R12_1.csv") %>%
  tibble::as_tibble()
usethis::use_data(cit_A1R12_1, overwrite = TRUE)

cit_A1R12_2 <-
  read.csv("inst/extdata/misosoup/cit_A1R12_2.csv") %>%
  tibble::as_tibble()
usethis::use_data(cit_A1R12_2, overwrite = TRUE)

ac_A3R04_1 <-
  read.csv("inst/extdata/misosoup/ac_A3R04_1.csv") %>%
  tibble::as_tibble()
usethis::use_data(ac_A3R04_1, overwrite = TRUE)

misosoup <- list()
for (f in fs::dir_ls("inst/extdata/misosoup")) {
  name <- f %>% fs::path_file() %>% fs::path_ext_remove()
  tb <- read.csv(f) %>% tibble::as_tibble()
  misosoup[[name]] <- MiCo(data = tb, name = name)
}
usethis::use_data(misosoup, overwrite = TRUE)
