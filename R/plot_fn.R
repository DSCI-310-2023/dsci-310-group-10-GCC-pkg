#' Line plot function
#'
#' This function plots a line graph given the data and the column names to
#' visualize and enables to customize the graphs axis labels and plot dimensions
#'
#' @param data The data which should be visualized
#' @param plot_width The width of the plot
#' @param plot_height The height of the plot
#' @param x_axis_data The data which should be displayed on the x axis
#' @param y_axis_data The data which should be displayed on the y axis
#' @param x_axis_label Label which will be displayed in the plot for the x axis
#' @param y_axis_label Label which will be displayed in the plot for the y axis
#'
#' @return The generated plot
#'
#' @export
#'
#' @examples
#' plot_line_graph(
#'   data = data.frame(neighbors = c(1, 2, 3), mean = c(0.80, 0.85, 0.95)),
#'   plot_width = 10, plot_height = 10,
#'   x_axis_data = neighbors, y_axis_data = mean, x_axis_label = "Neighbors",
#'   y_axis_label = "Accuracy Estimate"
#' )
plot_line_graph <- function(
    data, plot_width, plot_height, x_axis_data,
    y_axis_data, x_axis_label, y_axis_label) {
  # Check if all parameters are passed
  # Adapted from https://stackoverflow.com/questions/38758156/
  # r-check-if-any-missing-arguments
  defined <- ls()
  passed <- names(as.list(match.call())[-1])

  if (any(!defined %in% passed)) {
    stop(paste("missing values for", paste(setdiff(defined, passed),
      collapse = ","
    )))
  }

  options(repr.plot.width = plot_width, repr.plot.height = plot_height)

  return(ggplot2::ggplot(data, ggplot2::aes(x = {{ x_axis_data }}, y = {{ y_axis_data }})) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(x = x_axis_label, y = y_axis_label))
}



#' Scatter plot function
#'
#' This function plots a scatter graph given the data and the column names to
#' visualize and enables to customize the graphs axis labels and plot dimensions
#'
#' @param data The data which should be visualized
#' @param plot_width The width of the plot
#' @param plot_height The height of the plot
#' @param x_axis_data The data which should be displayed on the x axis
#' @param y_axis_data The data which should be displayed on the y axis
#' @param x_axis_label Label which will be displayed in the plot for the x axis
#' @param y_axis_label Label which will be displayed in the plot for the y axis
#' @param text_size Text size applied for all text in the plot
#' @param color Name of the label column of the dataset
#' @param color_label Defines labels for each of the colors
#'
#' @return The generated plot
#'
#' @export
#'
#' @examples
#' plot_scatter_graph(
#'   data = data.frame(
#'     ISI = c(5.2, 2.1, 5.2), BUI = c(10.1, 2.1, 4.2),
#'     Classes = c("fire", "fire", "no fire")
#'   ),
#'   plot_width = 10, plot_height = 10, x_axis_data = ISI,
#'   y_axis_data = BUI, x_axis_label = "ISI",
#'   y_axis_label = "BUI", text_size = 20, color = Classes,
#'   color_label = "Presence of fire"
#' )
plot_scatter_graph <- function(
    data, plot_width, plot_height, x_axis_data,
    y_axis_data, x_axis_label, y_axis_label, text_size, color, color_label) {
  # Check if all parameters are passed
  # Adapted from https://stackoverflow.com/questions/38758156/
  # r-check-if-any-missing-arguments
  defined <- ls()
  passed <- names(as.list(match.call())[-1])

  if (any(!defined %in% passed)) {
    stop(paste("missing values for", paste(setdiff(defined, passed),
      collapse = ","
    )))
  }

  options(repr.plot.width = plot_width, repr.plot.height = plot_height)

  return(ggplot2::ggplot(data, ggplot2::aes(
    x = {{ x_axis_data }}, y = {{ y_axis_data }},
    color = {{ color }}
  )) +
    ggplot2::geom_point() +
    ggplot2::labs(x = x_axis_label, y = y_axis_label, color = color_label) +
    ggplot2::theme(text = ggplot2::element_text(size = text_size)))
}
