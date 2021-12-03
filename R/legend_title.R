#' Select a legend title
#'
#' \code{legend_title()} selects a title from a list depending on the input.
#'
#' This function is to return a legend title that corresponds with the variable
#' chosen to color data points by.
#'
#' @param c An input that determines which title is returned
#'
#' @return A character string representing a legend title
#' @export
#'
legend_title <- function(c){
  switch(c,
         "None" = "None",
         "Severity" = "Accident Severity",
         "Temperature (F)" = "Temperature (F)",
         "Precipitation" = "Precipitation",
         "Day/Night" = "Day/Night",
         "Visibility" = "Visibility",
  )}
