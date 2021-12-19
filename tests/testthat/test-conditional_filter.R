test_that("When condition is not met conditional_filter() returns TRUE", {
  # Assign a value that does not match the condition
  x <- "color"

  expect_equal(conditional_filter(x == "size", x), TRUE)
})

test_that("When condition is met conditoinal_filter() returns the value of the success argument", {
  # Assign a value that matches the condition
  x <- "size"

  expect_equal(conditional_filter(x == "size", x), "size")
})
