test_that("alignment scoring produces expected results", {
  
  # Set 1
  tb1 <- readr::read_rds(test_path("fixtures", "aligment_test_set1.rds"))
  a1 <- newMiCoAl(newMiCo(tb1, name = "t1"), newMiCo(tb1, name = "t2"))
  s1 <- .scoreAlignment(a1@alignment)
  expect_equal(nrow(s1), 1)
  expect_equal(s1$score, 1)

  # Set 2
  a2 <- 
    readr::read_rds(test_path("fixtures", "aligment_test_set2.rds")) %>% 
    purrr::imap(newMiCo) %>% 
    newMiCoAl()
  s2 <- .scoreAlignment(a2@alignment)
  expect_equal(nrow(s2), 1)
  expect_equal(s2$score, 0.5)
})
