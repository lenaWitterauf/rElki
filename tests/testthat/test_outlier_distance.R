library(rElki)


testthat::test_that('knn_outlier', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::knn_outlier(test_data, 3)
  testthat::expect_is(test_result, 'list')
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('knn_weight_outlier', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::knn_weight_outlier(test_data, 3)
  testthat::expect_is(test_result, 'list')
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('odin', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::odin(test_data, 3)
  testthat::expect_is(test_result, 'list')
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})