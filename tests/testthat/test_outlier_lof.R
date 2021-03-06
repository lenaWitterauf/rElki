library(rElki)


testthat::test_that('lof', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::lof(test_data, 3)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('simplified_lof', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::simplified_lof(test_data, 3)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('cof', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::cof(test_data, 3)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('inflo', {
  test_data          <- replicate(5, rnorm(20))
  test_result        <- rElki::inflo(test_data, 3)
  test_result_with_m <- rElki::inflo(test_data, 3, 0.7)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_length(test_result_with_m, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('loop', {
  test_data               <- replicate(5, rnorm(20))
  test_result             <- rElki::loop(test_data, 3)
  test_result_with_lambda <- rElki::loop(test_data, 3, 9, 0.1)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_length(test_result_with_lambda, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('loop kreach', {
  test_data                    <- replicate(5, rnorm(20))
  test_result_default_kreach   <- rElki::loop(test_data, 3)
  test_result_with_same_kreach <- rElki::loop(test_data, 3, 3)
  test_result_with_diff_kreach <- rElki::loop(test_data, 3, 10)
  testthat::expect_length(test_result_default_kreach, nrow(test_data))
  testthat::expect_length(test_result_with_same_kreach, nrow(test_data))
  testthat::expect_length(test_result_with_diff_kreach, nrow(test_data))
  testthat::expect_equal(test_result_default_kreach, test_result_with_same_kreach)
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('ldof', {
  test_data   <- replicate(5, rnorm(20))
  test_result <- rElki::ldof(test_data, 3)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('ldf', {
  test_data            <- replicate(5, rnorm(20))
  test_result          <- rElki::ldf(test_data, 3)
  test_result_with_h   <- rElki::ldf(test_data, 3, h = 0.1)
  test_result_with_c   <- rElki::ldf(test_data, 3, c = 0.7)
  test_result_with_h_c <- rElki::ldf(test_data, 3, 0.2,  0.4)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_length(test_result_with_h, nrow(test_data))
  testthat::expect_length(test_result_with_c, nrow(test_data))
  testthat::expect_length(test_result_with_h_c, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('kdeos', {
  test_data              <- replicate(5, rnorm(20))
  test_result            <- rElki::kdeos(test_data, 3)
  test_result_with_scale <- rElki::kdeos(test_data, 3, kernel_scale = 0.8)
  test_result_with_bw    <- rElki::kdeos(test_data, 3, min_bandwidth = 0.2)
  test_result_with_idim  <- rElki::kdeos(test_data, 3, idim = 1)
  testthat::expect_length(test_result, nrow(test_data))
  testthat::expect_length(test_result_with_scale, nrow(test_data))
  testthat::expect_length(test_result_with_bw, nrow(test_data))
  testthat::expect_length(test_result_with_idim, nrow(test_data))
  testthat::expect_null(rJava::.jgetEx())
})

testthat::test_that('kdeos kmin kmax', {
  test_data                <- replicate(5, rnorm(20))
  test_result_default_kmax <- rElki::kdeos(test_data, 3)
  test_result_same_kmax    <- rElki::kdeos(test_data, 3, k_max = 3)
  test_result_diff_kmax    <- rElki::kdeos(test_data, 3, k_max = 5)
  testthat::expect_length(test_result_default_kmax, nrow(test_data))
  testthat::expect_length(test_result_same_kmax, nrow(test_data))
  testthat::expect_length(test_result_diff_kmax, nrow(test_data))
  testthat::expect_equal(test_result_default_kmax, test_result_same_kmax)
  testthat::expect_null(rJava::.jgetEx())
})