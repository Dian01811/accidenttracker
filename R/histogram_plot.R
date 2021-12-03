#' Choose to plot histogram or bar plot
#'
#' \code{histogram_plot()} plots a histogram if a continuous variable from \code{accidents}
#' is chosen or a bar plot if a categorical variable is chosen.
#'
#' Creates a bar plot if a categorical variable is chosen to prevent issues
#' with plotting non-continuous data as a histogram.
#'
#' @param data A dataframe, specifically the \code{accidents} dataset
#' @param x A variable from \code{accidents} to plot
#' @param y The number of bins
#'
#' @return A histogram plot if continuous or a bar plot if categorical
#' @export

histogram_plot <- function(data, x, y){
  if(x == "state" | x == "day.night" | x == "wind.dir" | x == "side" | x == "month" | x == "hour"){
    ggplot2::ggplot(data, ggplot2::aes_string(x)) +
      ggplot2::geom_bar(width=1)
  }
  else{
    ggplot2::ggplot(data, ggplot2::aes_string(x)) +
      ggplot2::geom_histogram(bins = y)
  }
}
