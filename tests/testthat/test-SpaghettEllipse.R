# test-SpaghettEllipse.R
library(testthat)

test_that("SpaghettEllipse checks for required columns", {
  df <- data.frame(
    participant_id = rep("P1", 10),
    Time = seq(0, 0.9, by = 0.1),
    CoP_X = rnorm(10),
    CoP_Y = rnorm(10)
  )

  # Missing time_col
  expect_error(
    SpaghettEllipse(df, participant_id_col = "participant_id", time_col = "Wrong_Time"),
    "The following specified columns do not exist in the dataframe: Wrong_Time"
  )

  # Missing CoP_X
  expect_error(
    SpaghettEllipse(df, participant_id_col = "participant_id", copx_col = "Wrong_CoP_X"),
    "The following specified columns do not exist in the dataframe: Wrong_CoP_X"
  )

  # Missing CoP_Y
  expect_error(
    SpaghettEllipse(df, participant_id_col = "participant_id", copy_col = "Wrong_CoP_Y"),
    "The following specified columns do not exist in the dataframe: Wrong_CoP_Y"
  )
})

test_that("SpaghettEllipse filters by participant_id if provided", {
  df <- data.frame(
    participant_id = rep(c("P1", "P2"), each = 10),
    Time = rep(seq(0, 0.9, by = 0.1), 2),
    CoP_X = rnorm(20),
    CoP_Y = rnorm(20)
  )

  # Specifying participant_id = "P1" should subset the data
  # As we cannot test the plot visually, we just ensure no error
  expect_silent(SpaghettEllipse(df, participant_id_col = "participant_id", participant_id = "P1"))

  # Non-existent participant
  expect_error(
    SpaghettEllipse(df, participant_id_col = "participant_id", participant_id = "P3"),
    "No data found for the specified participant ID."
  )
})

test_that("SpaghettEllipse handles insufficient data", {
  df <- data.frame(
    participant_id = rep("P1", 1),
    Time = 0.1,
    CoP_X = 0.5,
    CoP_Y = -0.5
  )

  # With only one data point, it cannot compute covariance
  expect_error(
    SpaghettEllipse(df, participant_id_col = "participant_id"),
    "Not enough data to compute covariance and plot the ellipse."
  )
})

test_that("SpaghettEllipse runs without error for valid input", {
  set.seed(123)
  df <- data.frame(
    participant_id = rep("P1", 100),
    Time = seq(0, 9.9, by = 0.1),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  # We only check that it runs without error, as testing the plot visually isn't possible here
  expect_silent(SpaghettEllipse(df, participant_id_col = "participant_id"))
})

test_that("SpaghettEllipse warns if covariance matrix is singular", {
  # Create data where CoP_X and CoP_Y are identical => covariance matrix singular
  df <- data.frame(
    participant_id = rep("P1", 50),
    Time = seq(0, 4.9, by = 0.1),
    CoP_X = rep(1, 50),
    CoP_Y = rep(1, 50)
  )

  expect_warning(
    SpaghettEllipse(df, participant_id_col = "participant_id"),
    "Covariance matrix is singular or not positive-definite. Cannot compute ellipse."
  )
})

test_that("SpaghettEllipse applies specified confidence level", {
  # We cannot check the plot, but we can run the function and ensure no error
  set.seed(123)
  df <- data.frame(
    participant_id = rep("P1", 100),
    Time = seq(0, 9.9, by = 0.1),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  # Different confidence levels
  expect_silent(SpaghettEllipse(df, participant_id_col = "participant_id", conf_level = 0.8))
  expect_silent(SpaghettEllipse(df, participant_id_col = "participant_id", conf_level = 0.99))
})

test_that("SpaghettEllipse title and axis labels can be customized", {
  set.seed(123)
  df <- data.frame(
    participant_id = rep("P1", 50),
    Time = seq(0, 4.9, by = 0.1),
    CoP_X = rnorm(50),
    CoP_Y = rnorm(50)
  )

  expect_silent(SpaghettEllipse(df,
                                participant_id_col = "participant_id",
                                Title = "Custom Title",
                                xlab = "X-Axis Label",
                                ylab = "Y-Axis Label"))
})
