misosoup24 <- list()
for (f in fs::dir_ls("inst/extdata/misosoup")) {
  name <- f %>%
    fs::path_file() %>%
    fs::path_ext_remove()
  tb <- read.csv(f) %>% tibble::as_tibble()
  misosoup24[[name]] <- tb
}
usethis::use_data(misosoup24, overwrite = TRUE)
