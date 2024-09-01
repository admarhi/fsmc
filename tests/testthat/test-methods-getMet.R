test_that("returns correct metabolites from MiCoAl", {
  mets <- unique(cit_A1R12_1$metabolites)
  mets_MiCo <- newMiCo(cit_A1R12_1) %>% getMet()

  expect_true(all(mets %in% mets_MiCo))
})


test_that("Returns union of metabolites from MiCoAl", {
  c1 <- newMiCo(cit_A1R12_1)
  c2 <- newMiCo(cit_A1R12_2)
  mets_union <- union(getMet(c1), getMet(c2))
  c_alig <- newMiCoAl(c1, c2)
  expect_true(all(getMet(c_alig) %in% mets_union))
})
