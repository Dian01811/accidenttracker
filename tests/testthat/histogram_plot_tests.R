library(tidyverse)
library(ggplot2)

test_that("When state  is passed into histogram_plot, a ggplot object with a geom_bar is returned.", {
  # Act: Get actual values from histogram_plot function. 
  actual <- histogram_plot("state", 5)
  
  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "state",
               ignore_attr = TRUE)
})

test_that("When day.night is passed into histogram_plot, a ggplot object with a geom_bar is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("day.night", 5)

  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "day.night",
               ignore_attr = TRUE)
})

test_that("When wind.dir is passed into histogram_plot, a ggplot object with a geom_bar is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("wind.dir", 5)

  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "wind.dir",
               ignore_attr = TRUE)
})

test_that("When side is passed into histogram_plot, a ggplot object with a geom_bar is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("side", 5)

  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "side",
               ignore_attr = TRUE)
})

test_that("When month is passed into histogram_plot, a ggplot object with a geom_bar is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("month", 5)

  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "month",
               ignore_attr = TRUE)
})

test_that("When hour is passed into histogram_plot, a ggplot object with a geom_bar is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("hour", 5)

  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "hour",
               ignore_attr = TRUE)
})

test_that("When hour is passed into histogram_plot, a ggplot object with a geom_histogram is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("abc", 5)

  # Assert: Assert actual is equal to expected
  expect_s3_class(actual, "ggplot")
  expect_equal(actual[[9]]$x, 
               "abc",
               ignore_attr = TRUE)
})

