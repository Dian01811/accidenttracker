library(tidyverse)
library(accidenttracker)

test_that("When x == state, then the proper bar graph is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("state", 5)

  # Assert: Assert actual is equal to expected
  expect_equal(actual,
               ggplot(accidents, aes_string("state")) +
                 geom_bar(width=1),
               ignore_attr = TRUE)
})

test_that("When x == day.night, then the proper bar graph is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("day.night", 5)

  # Assert: Assert actual is equal to expected
  expect_equal(actual,
               ggplot(accidents, aes_string("day.night")) +
                 geom_bar(width=1),
               ignore_attr = TRUE)
})

test_that("When x == wind.dir, then the proper bar graph is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("wind.dir", 5)

  # Assert: Assert actual is equal to expected
  expect_equal(actual,
               ggplot(accidents, aes_string("wind.dir")) +
                 geom_bar(width=1),
               ignore_attr = TRUE)
})

test_that("When x == side, then the proper bar graph is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("side", 5)

  # Assert: Assert actual is equal to expected
  expect_equal(actual,
               ggplot(accidents, aes_string("side")) +
                 geom_bar(width=1),
               ignore_attr = TRUE)
})

test_that("When x == month, then the proper bar graph is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("month", 5)

  # Assert: Assert actual is equal to expected
  expect_equal(actual,
               ggplot(accidents, aes_string("month")) +
                 geom_bar(width=1),
               ignore_attr = TRUE)
})

test_that("When x == hour, then the proper bar graph is returned.", {
  # Act: Get actual values from histogram_plot function.
  actual <- histogram_plot("hour", 5)

  # Assert: Assert actual is equal to expected
  expect_equal(actual,
               ggplot(accidents, aes_string("hour")) +
                 geom_bar(width=1),
               ignore_attr = TRUE)
})
