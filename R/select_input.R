#' Create input for a shiny app
#'
#' @param inputId A shiny input ID
#' @param label A label for the input
#' @param choices A vector of choices for the input
#' @param type The type of input, select for selectInput, multiple for selectInput multiple = TRUE, and checkbox for checkboxGroupInput
#' @return A shiny widget that allows user to select from a list of choices
#' @examples
#' select_input("color", "Color By:", c("None", "Temp", "Day/Night"), "select")

select_input <- function(inputId, label, choices, type) {
  if (type == "select") {
    shiny::selectInput(inputId, label, choices)
  } else if (type == "multiple") {
    shiny::selectInput(inputId, label, choices, multiple = TRUE)
  } else if (type == "checkbox") {
    shiny::checkboxGroupInput(inputId, label, choices)
  } else {
    stop("Invalid type, must choose from select, multiple, or checkbox")
  }
}
