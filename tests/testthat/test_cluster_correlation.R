library(rElki)


testthat::test_that('cash', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::cash(test_data, minpts = 5, maxlevel = 10, jitter = 0.1, adjust = TRUE)
  testthat::expect_null(rJava::.jgetEx())
})