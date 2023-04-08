library(dplyr)
library(forcats)

# import a data frame to be tested and apply the function to the data frame
df2_test <- FFire::df_load(
  url = "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv",
  skip1 = 0, skip2 = 0, n_max1 = 3, n_max2 = 2, error_line = 2,
  error_record = 2:4, correct_bef_error_record = 1,
  val_corrected = list(3.5, 1.4, 0.2, "setosa"),
  error_col = c("petal_width"), predicted_factor = "species"
)


# define the expected output
df2_expect <- data.frame(
  sepal_length = c(5.1, 4.9, 5.1, 4.9, 4.7),
  sepal_width = c(3.5, 3.5, 3.5, 3, 3.2),
  petal_length = c(1.4, 1.4, 1.4, 1.4, 1.3),
  petal_width = c(0.2, 0.2, 0.2, 0.2, 0.2),
  species = c("setosa", "setosa", "setosa", "setosa", "setosa")
) %>%
  dplyr::mutate(species = forcats::as_factor(species))

# check expected
testthat::test_that("`df_load` should return a data frame", {
  testthat::expect_equal(df2_test, df2_expect, ignore_attr = TRUE)
})

testthat::test_that("`df_load` should throw error if a non-numeric value is passed
          for variables which should be numeric", {
  expected_error <- "`n_max1`, `n_max2`, `skip1`, `skip2`, `error_line` and
            `correct_bef_error_record` should be numeric"
  testthat::expect_error(FFire::df_load(
    url = "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv",
    skip1 = 0, skip2 = 0, n_max1 = 3, n_max2 = 2, error_line = "hello",
    error_record = 2:4, correct_bef_error_record = 1,
    val_corrected = list(3.5, 1.4, 0.2, "setosa"),
    error_col = c("petal_width"), predicted_factor = "species"
  ), expected_error)
})

testthat::test_that("`df_load` should throw error if a non-string value is passed
          for variables which should be a string", {
  expected_error <- "`predicted_factor` should be a string"
  testthat::expect_error(FFire::df_load(
    url = "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv",
    skip1 = 0, skip2 = 0, n_max1 = 3, n_max2 = 2, error_line = 2,
    error_record = 2:4, correct_bef_error_record = 1,
    val_corrected = list(3.5, 1.4, 0.2, "setosa"),
    error_col = c("petal_width"), predicted_factor = 2
  ), expected_error)
})
