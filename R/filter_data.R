library(dplyr)
library(purrr)

#' Filter data function
#'
#' This function calculates metrics based on a given algorithm
#' (e.g. mean, range, etc.) on the used dataset (e.g. fire_train)
#' filters the data by a given column name and a value and by a
#' column range
#'
#' @param data Data to be filtered
#'
#' @param col_to_filter_by Name of the column which should be used
#' for filtering the data.
#'
#' @param subset The filtering condition which the targeted function
#' should follow.
#'
#' @param alg  The filtering algorithm which the targeted function
#' should follow.
#'
#' @param start_col The start of the column range which should be
#' selected from the data.
#'
#' @param end_col The end of the column range which should be
#' selected from the data.
#'
#' @return Return the data filtered by the parameters above.
#'
#' @export
#'
#' @examples
#' filter_data(
#'   data.frame(
#'     type = c("big", "big", "small"),
#'     length = c(5.2, 2.1, 1.2),
#'     height = c(10.1, 20.1, 4.2),
#'     depth = c(3, 2, 1.8)
#'   ), type, "big", height, depth, mean
#' )
filter_data <- function(
    data, col_to_filter_by, subset,
    start_col, end_col, alg) {
  filtered_data <- data %>%
    dplyr::filter({{ col_to_filter_by }} == {{ subset }}) %>%
    dplyr::select({{ start_col }}:{{ end_col }}) %>%
    purrr::map_df({{ alg }})
  return(filtered_data)
}
