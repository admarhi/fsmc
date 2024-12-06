test_that("alignment scoring produces expected results", {
  # Set 1
  tb1 <- readr::read_rds(test_path("fixtures", "aligment_test_set1.rds"))
  a1 <- MicrobiomeFunctionAlignment(
    MicrobiomeFunction(tb1, name = "t1"),
    MicrobiomeFunction(tb1, name = "t2"),
    name = "test"
  )
  s1 <- a1@Score
  expect_equal(nrow(s1), 1)
  expect_equal(s1$score, 1)

  # Set 2
  a2 <-
    readr::read_rds(test_path("fixtures", "aligment_test_set2.rds")) %>%
    purrr::imap(MicrobiomeFunction) %>%
    MicrobiomeFunctionAlignment(name = "test")
  s2 <- a2@Score
  expect_equal(nrow(s2), 1)
  expect_equal(s2$score, 0.5)

  # Set 3
  l3 <-
    readr::read_rds(test_path("fixtures", "aligment_test_set3.rds")) %>%
    purrr::imap(MicrobiomeFunction)

  a3 <- MicrobiomeFunctionAlignment(l3, name = "test")
  s3 <- a3@Score
  expect_equal(nrow(s3), 2)
  expect_contains(s3$score, 1 / 6)

  a3.1 <- MicrobiomeFunctionAlignment(l3[1:2], name = "test")
  s3.1 <- a3.1@Score
  expect_equal(nrow(s3.1), 1)
  expect_equal(s3.1$score, 4 / 6)

  # Set 4
  a4 <-
    readr::read_rds(test_path("fixtures", "aligment_test_set4.rds")) %>%
    purrr::imap(MicrobiomeFunction) %>%
    MicrobiomeFunctionAlignment(name = "test")

  s4 <- a4@Score
  expect_equal(nrow(s4), 3)
  expect_equal(s4$score, c(1 / 2, 1 / 2, 5 / 6 * 2 / 4))

  # Set 5
  a5 <-
    readr::read_rds(test_path("fixtures", "aligment_test_set5.rds")) %>%
    purrr::imap(MicrobiomeFunction) %>%
    MicrobiomeFunctionAlignment(name = "test")

  s5 <- a5@Score
  expect_equal(nrow(s5), 3)
  expect_equal(round(s5$score, 2), c(0.43, 0.64, 0.5))
})
