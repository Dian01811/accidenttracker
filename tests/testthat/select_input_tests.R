library(shiny)

# Arrange: Arranging values to be used in the tests.
inputId <- "color"
label <- "Color the data by"
choices <- c("a", "b", "c")

test_that("When type == select, then the proper selectInput is returned.", {
  # Act: Get actual shiny::selectInput object from the select_input function.
  actual <- select_input(inputId, label, choices, type = "select")
  
  # Assert: Actual select_input is equal to shiny::selectInput
  expect_equal(actual, 
               shiny::selectInput(inputId, label, choices), 
               ignore_attr = TRUE)
})

test_that("When type == multiple, then the proper selectInput is returned.", {
  # Act: Get actual shiny::selectInput object from the select_input function.
  actual <- select_input(inputId, label, choices, type = "multiple")
  
  # Assert: Actual select_input is equal to shiny::selectInput
  expect_equal(actual, 
               shiny::selectInput(inputId, label, choices, multiple = TRUE), 
               ignore_attr = TRUE)
})

test_that("When type == checkbox, then the proper selectInput is returned.", {
  # Act: Get actual shiny::selectInput object from the select_input function.
  actual <- select_input(inputId, label, choices, type = "checkbox")
  
  # Assert: Actual select_input is equal to shiny::selectInput
  expect_equal(actual, 
               shiny::checkboxGroupInput(inputId, label, choices), 
               ignore_attr = TRUE)
})

