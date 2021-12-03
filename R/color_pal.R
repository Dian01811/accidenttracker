#' Select a color palette
#'
#' \code{color_pal()} returns a color palette with values assigned to the values
#' of a column in the \code{accidents} dataset.
#'
#' This function returns a color palette based on the input in the \code{a} argument.
#' It is designed to color markers for a leaflet map in a shiny app based on user selection.
#'
#' @param data A dataframe, specifically the \code{accidents} dataset
#' @param a A character string
#'
#' @return A color palette assigned to the values of a column in the \code{accidents} dataset
#' @export
#'
color_pal <- function(data, a){
  switch(a,
         "None" =  leaflet::colorBin("black", domain = NULL),
         "Severity" = leaflet::colorFactor("YlOrRd", domain = data$severity),
         "Temperature (F)" = leaflet::colorBin("RdBu", reverse = TRUE, domain = data$temp,
                                      bins = c(-50, 0, 32, 50, 80, 100, 175)),
         "Precipitation" = leaflet::colorBin("BrBG", domain = data$precip,
                                    bins = c(0, 0.001, 0.05, 0.1, 0.3, 1, 2.0)),
         "Day/Night" = leaflet::colorFactor("Dark2", domain = data$day.night),
         "Visibility" = leaflet::colorBin("YlGnBu", domain = data$vis),
                                 bins = c(0, 0.5, 2, 150)
         )
}
