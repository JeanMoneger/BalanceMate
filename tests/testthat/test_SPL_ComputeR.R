# test-SPL_ComputeR.R
library(testthat)

test_that("SPL_ComputeR checks for required columns", {
  df <- data.frame(
    ID = 1:10,
    CoP_X = rnorm(10),
    CoP_Y = rnorm(10)
  )

  expect_error(
    SPL_ComputeR(df, CoPX_col = "Wrong_X", CoPY_col = "CoP_Y", ID = "ID"),
    "Column Wrong_X not found in the data frame"
  )

  expect_error(
    SPL_ComputeR(df, CoPX_col = "CoP_X", CoPY_col = "Wrong_Y", ID = "ID"),
    "Column Wrong_Y not found in the data frame"
  )

  expect_error(
    SPL_ComputeR(df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "Wrong_ID"),
    "Column Wrong_ID not found in the data frame"
  )
})

test_that("SPL_ComputeR works without epochs", {
  set.seed(123)
  df <- data.frame(
    participantID = rep(c("A", "B"), each = 50),
    CoP_X = rnorm(100, mean = 0),
    CoP_Y = rnorm(100, mean = 0)
  )

  result <- SPL_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "participantID")

  # Expect one row per participant
  expect_equal(nrow(result), 2)
  expect_true("participant_id" %in% names(result))
  expect_true("sway_path_length" %in% names(result))
  expect_false("epoch" %in% names(result))
})

test_that("SPL_ComputeR works with epochs", {
  set.seed(123)
  df <- data.frame(
    ID = rep("A", 100),
    Time = seq(0, 9.9, by = 0.1),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  # epoch_length = 2 s => 5 epochs in 10 s
  result <- SPL_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y",
                         ID = "ID", time_col = "Time", epoch_length = 2)

  # Expect 5 epochs
  expect_equal(nrow(result), 5)
  expect_true("epoch" %in% names(result))
  expect_equal(length(unique(result$epoch)), 5)
})

test_that("SPL_ComputeR warns if epochs are unbalanced", {
  set.seed(123)
  # Total duration not divisible by epoch length perfectly
  # e.g., 10.5 seconds total with a 2 s epoch
  df <- data.frame(
    ID = rep("A", 105),
    Time = seq(0, 10.4, by = 0.1),
    CoP_X = rnorm(105),
    CoP_Y = rnorm(105)
  )

  expect_warning(
    SPL_ComputeR(df, "CoP_X", "CoP_Y", "ID", time_col = "Time", epoch_length = 2),
    "Unbalanced epochs detected"
  )
})

test_that("SPL_ComputeR computes correct sway path length for known data", {
  # Known scenario:
  # Imagine points on a simple line on X-axis: X = 1,2,3,..., Y=0
  # Distances = (2-1)+(3-2)+(4-3)... = total = last_x - first_x = 9 for 1:10
  df <- data.frame(
    ID = rep("A", 10),
    CoP_X = 1:10,
    CoP_Y = rep(0, 10)
  )

  result <- SPL_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")
  # Expected distance: from 1 to 10 is a series of steps each of length 1, 9 intervals *1 = 9
  expect_equal(result$sway_path_length, 9)
})

test_that("SPL_ComputeR handles multiple participants and orders correctly", {
  # Two participants out of alphabetical order
  df <- data.frame(
    ID = c(rep("B", 10), rep("A", 10)),
    CoP_X = c(1:10, 1:10),
    CoP_Y = 0
  )

  result <- SPL_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  # Sorted by participant_id => A first, B next
  expect_equal(result$participant_id, c("A", "B"))
  # Both have the same path length: 9
  expect_equal(result$sway_path_length, c(9, 9))
})

test_that("SPL_ComputeR returns no rows if not enough points in a group", {
  # If a participant or an epoch has only one data point, can't compute distance
  df <- data.frame(
    ID = c("A", "A", "B"),
    CoP_X = c(1, 2, 3),
    CoP_Y = c(0, 0, 0)
  )

  # Participant A has 2 points => path length computed
  # Participant B has only 1 point => NA returned, filtered out
  result <- SPL_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  expect_equal(nrow(result), 1)
  expect_equal(result$participant_id, "A")
  expect_equal(result$sway_path_length, (2-1)) # = 1
})
