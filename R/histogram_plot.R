#' Switch between histogram or bar plot based on type of data
#'
#' @param x A variable to plot
#' @param y The number of bins
#' @return A histogram plot if continuous or a bar plot if categorical
#' @export

histogram_plot <- function(x, y){
  if(x == "state" | x == "day.night" | x == "wind.dir" | x == "side" | x == "month" | x == "hour"){
    ggplot(accidents, aes_string(x)) +
      geom_bar(width=1)
  }
  else{
    ggplot(accidents, aes_string(x)) +
      geom_histogram(bins = y)
  }
}
