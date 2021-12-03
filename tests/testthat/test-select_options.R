library(dplyr)

# Arrange: Arranging values to test function 'select_options'.
column <- c("value 3", "value 2", "value 2", "value 1")
column2 <- c("value 1")
df <- data.frame("column.header" = column, "column2.header" = column2)


test_that("When all = FALSE and none = FALSE, then returns unique sorted column.",{
  # Act: Get the actual values from select_options function.
  actual <- select_options(df, "column.header")

  # Assert: Assert actual is equal to expected.
  expect_type(actual, "character")
  expect_equal(actual, c("value 1", "value 2", "value 3"))
  expect_vector(actual, size = 3)
})

test_that("When all = TRUE and none = FALSE, then returns All + unique sorted column.",{
  # Act: Get the actual values from select_options function.
  actual <- select_options(df, "column.header", all = TRUE)

  # Assert: Assert actual is equal to expected.
  expect_type(actual, "character")
  expect_equal(actual, c("All", "value 1", "value 2", "value 3"))
  expect_vector(actual, size = 4)
})

test_that("When all = FALSE and none = TRUE, then returns None + unique sorted column.",{
  # Act: Get the actual values from select_options function.
  actual <- select_options(df, "column.header", none = TRUE)

  # Assert: Assert actual is equal to expected.
  expect_type(actual, "character")
  expect_equal(actual, c("None", "value 1", "value 2", "value 3"))
  expect_vector(actual, size = 4)
})

