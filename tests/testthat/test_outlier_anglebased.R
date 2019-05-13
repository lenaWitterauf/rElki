library(rElki)


testthat::test_that('fast_abod', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::fast_abod(test_data, 3)
  testthat::expect_is(test_result, 'list')
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})