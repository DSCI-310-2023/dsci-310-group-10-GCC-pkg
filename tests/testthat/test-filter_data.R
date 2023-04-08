dummy_data = data.frame(
  type = c("big", "big", "small"),
  length = c(5.2, 2.1, 1.2),
  height = c(10.1, 20.1, 4.2),
  depth = c(3, 2, 1.8)
  )

# Testing
testthat::test_that("Test if filter_data returns two rows if alg=range", {
  testthat::expect_equal(nrow(FFire::filter_data(
    dummy_data, type, "big", height, depth, range
  )), 2)
})

# Testing
testthat::test_that("Test if filter_data returns two rows if alg=mean", {
  testthat::expect_equal(nrow(FFire::filter_data(
    dummy_data, type, "big", height, depth, mean
  )), 1)
})
