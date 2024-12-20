# test-Mean_CoP_ComputeR.R
library(testthat)

test_that("Mean_CoP_ComputeR checks for required columns", {
  df <- data.frame(
    ID = 1:10,
    CoP_X = rnorm(10),
    CoP_Y = rnorm(10)
  )

  # Missing CoPX_col
  expect_error(
    Mean_CoP_ComputeR(df, "Wrong_CoP_X", "CoP_Y", "ID"),
    "Column Wrong_CoP_X not found in the data frame"
  )

  # Missing CoPY_col
  expect_error(
    Mean_CoP_ComputeR(df, "CoP_X", "Wrong_CoP_Y", "ID"),
    "Column Wrong_CoP_Y not found in the data frame"
  )

  # Missing ID
  expect_error(
    Mean_CoP_ComputeR(df, "CoP_X", "CoP_Y", "Wrong_ID"),
    "Column Wrong_ID not found in the data frame"
  )
})

test_that("Mean_CoP_ComputeR works without epochs", {
  set.seed(123)
  df <- data.frame(
    participantID = rep(c("A", "B"), each = 50),
    CoP_X = rnorm(100, mean = 0.5),
    CoP_Y = rnorm(100, mean = -0.3)
  )

  result <- Mean_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "participantID")

  # Expect one row per participant
  expect_equal(nrow(result), 2)

  # Check column presence
  expect_true("participant_id" %in% names(result))
  expect_true("mean_CoPX" %in% names(result))
  expect_true("mean_CoPY" %in% names(result))

  # Means should be close to the expected distributions
  expect_true(abs(mean(df$CoP_X[df$participantID == "A"]) - result$mean_CoPX[result$participant_id == "A"]) < 1e-9)
  expect_true(abs(mean(df$CoP_Y[df$participantID == "B"]) - result$mean_CoPY[result$participant_id == "B"]) < 1e-9)
})

test_that("Mean_CoP_ComputeR works with epochs", {
  set.seed(123)
  df <- data.frame(
    ID = rep("A", 100),
    Time = seq(0, 9.9, by = 0.1),
    CoP_X = rnorm(100, mean = 0),
    CoP_Y = rnorm(100, mean = 0)
  )

  # epoch_length = 2 s => 5 epochs in 10 s
  result <- Mean_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y",
                              ID = "ID", time_col = "Time", epoch_length = 2)

  # Expect 5 epochs
  expect_equal(nrow(result), 5)
  expect_true("epoch" %in% names(result))
  expect_equal(length(unique(result$epoch)), 5)

  # Each epoch mean should match manual calculation
  for (e in 1:5) {
    epoch_indices <- ((e-1)*20 + 1):(e*20)
    expected_mean_x <- mean(df$CoP_X[epoch_indices])
    expected_mean_y <- mean(df$CoP_Y[epoch_indices])
    actual_mean_x <- result$mean_CoPX[result$epoch == e]
    actual_mean_y <- result$mean_CoPY[result$epoch == e]
    expect_equal(expected_mean_x, actual_mean_x)
    expect_equal(expected_mean_y, actual_mean_y)
  }
})

test_that("Mean_CoP_ComputeR warns if epochs are unbalanced", {
  set.seed(123)
  # Total duration not divisible by the epoch length precisely
  df <- data.frame(
    ID = rep("A", 105),
    Time = seq(0, 10.4, by = 0.1),
    CoP_X = rnorm(105),
    CoP_Y = rnorm(105)
  )

  expect_warning(
    Mean_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID", time_col = "Time", epoch_length = 2),
    "Unbalanced epochs detected"
  )
})

test_that("Mean_CoP_ComputeR handles missing values", {
  df <- data.frame(
    ID = rep("A", 10),
    CoP_X = c(NA, 1:9),
    CoP_Y = c(1, NA, 2:9)
  )

  result <- Mean_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  # Means should ignore NA
  expect_equal(result$mean_CoPX, mean(1:9, na.rm = TRUE))
  expect_equal(result$mean_CoPY, mean(c(1, NA, 2:9), na.rm = TRUE))
})

test_that("Mean_CoP_ComputeR returns NA if no rows in a group", {
  # Simulate an empty participant by filtering
  df <- data.frame(
    ID = c(rep("A", 10), rep("B", 0)),
    CoP_X = rnorm(10),
    CoP_Y = rnorm(10)
  )

  # Participant B has no data
  result <- Mean_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  expect_equal(nrow(result), 1) # Only A appears since B has no data
})

test_that("Mean_CoP_ComputeR ordering is correct", {
  # Two participants, out of alphabetical order
  df <- data.frame(
    ID = c(rep("B", 10), rep("A", 10)),
    CoP_X = rnorm(20),
    CoP_Y = rnorm(20)
  )

  result <- Mean_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  # Expect alphabetical order by participant_id
  expect_equal(result$participant_id, c("A", "B"))
})

