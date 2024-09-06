test_that("alignment scoring produces expected results", {
  
  # Read in the test alignment test files 
  tb_set1 <- readr::read_rds(test_path("fixtures", "alig_test_set1.rds"))
  
  a1 <- newMiCoAl(newMiCo(tb_set1, name = "t1"), newMiCo(tb_set1, name = "t2"))
  s1 <- .scoreAlignment(a1@alignment)
  expect_equal(nrow(s1), 1)
  expect_equal(s1$score, 1)
})
