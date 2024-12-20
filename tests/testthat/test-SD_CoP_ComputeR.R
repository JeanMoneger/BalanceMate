# test-SD_CoP_ComputeR.R
library(testthat)

test_that("SD_CoP_ComputeR checks for required columns", {
  df <- data.frame(
    ID = 1:10,
    CoP_X = rnorm(10),
    CoP_Y = rnorm(10)
  )

  # Missing CoPX_col
  expect_error(
    SD_CoP_ComputeR(df, "Wrong_CoP_X", "CoP_Y", "ID"),
    "Column Wrong_CoP_X not found in the data frame"
  )

  # Missing CoPY_col
  expect_error(
    SD_CoP_ComputeR(df, "CoP_X", "Wrong_CoP_Y", "ID"),
    "Column Wrong_CoP_Y not found in the data frame"
  )

  # Missing ID
  expect_error(
    SD_CoP_ComputeR(df, "CoP_X", "CoP_Y", "Wrong_ID"),
    "Column Wrong_ID not found in the data frame"
  )
})

test_that("SD_CoP_ComputeR works without epochs", {
  set.seed(123)
  df <- data.frame(
    participantID = rep(c("A", "B"), each = 50),
    CoP_X = rnorm(100, mean = 0),
    CoP_Y = rnorm(100, mean = 0)
  )

  result <- SD_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "participantID")

  # Expect one row per participant
  expect_equal(nrow(result), 2)

  # Check columns
  expect_true("participant_id" %in% names(result))
  expect_true("SD_CoPX" %in% names(result))
  expect_true("SD_CoPY" %in% names(result))

  # SD should match manual computation
  expect_equal(
    result$SD_CoPX[result$participant_id == "A"],
    sd(df$CoP_X[df$participantID == "A"], na.rm = TRUE)
  )
  expect_equal(
    result$SD_CoPY[result$participant_id == "B"],
    sd(df$CoP_Y[df$participantID == "B"], na.rm = TRUE)
  )
})

test_that("SD_CoP_ComputeR works with epochs", {
  set.seed(123)
  df <- data.frame(
    ID = rep("A", 100),
    Time = seq(0, 9.9, by = 0.1),
    CoP_X = rnorm(100, mean = 10),
    CoP_Y = rnorm(100, mean = -5)
  )

  # epoch_length = 2 s => 5 epochs in 10 s
  result <- SD_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y",
                            ID = "ID", time_col = "Time", epoch_length = 2)

  expect_equal(nrow(result), 5)
  expect_true("epoch" %in% names(result))
  expect_equal(length(unique(result$epoch)), 5)

  # Check correctness of SD for one epoch
  # For epoch 1, indices: 1:20 (because 2s at 100Hz = 200 samples/s * 2s = 20 samples)
  epoch_1_indices <- 1:20
  expected_SD_CoPX <- sd(df$CoP_X[epoch_1_indices], na.rm = TRUE)
  expected_SD_CoPY <- sd(df$CoP_Y[epoch_1_indices], na.rm = TRUE)
  actual_SD_CoPX <- result$SD_CoPX[result$epoch == 1]
  actual_SD_CoPY <- result$SD_CoPY[result$epoch == 1]

  expect_equal(expected_SD_CoPX, actual_SD_CoPX)
  expect_equal(expected_SD_CoPY, actual_SD_CoPY)
})

test_that("SD_CoP_ComputeR warns if epochs are unbalanced", {
  set.seed(123)
  df <- data.frame(
    ID = rep("A", 105),  # 10.5 seconds approx
    Time = seq(0, 10.4, by = 0.1),
    CoP_X = rnorm(105),
    CoP_Y = rnorm(105)
  )

  expect_warning(
    SD_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID", time_col = "Time", epoch_length = 2),
    "Unbalanced epochs detected"
  )
})

test_that("SD_CoP_ComputeR handles missing values", {
  df <- data.frame(
    ID = rep("A", 10),
    CoP_X = c(NA, rnorm(9)),
    CoP_Y = c(rnorm(9), NA)
  )

  result <- SD_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  # Check if the NAs are handled correctly in the SD calculation
  expect_equal(result$SD_CoPX, sd(df$CoP_X, na.rm = TRUE))
  expect_equal(result$SD_CoPY, sd(df$CoP_Y, na.rm = TRUE))
})

test_that("SD_CoP_ComputeR returns NA if only one data point in a group", {
  # If a group (e.g., participant or epoch) has only 1 data point, SD is NA
  df <- data.frame(
    ID = c(rep("A", 2), rep("B", 1)), # B has only 1 data point
    CoP_X = c(1, 2, 3),
    CoP_Y = c(2, 4, 6)
  )

  result <- SD_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  # For participant B, SD should be NA
  expect_true(is.na(result$SD_CoPX[result$participant_id == "B"]))
  expect_true(is.na(result$SD_CoPY[result$participant_id == "B"]))
})

test_that("SD_CoP_ComputeR ordering is correct", {
  # Two participants out of order
  df <- data.frame(
    ID = c(rep("B", 10), rep("A", 10)),
    CoP_X = rnorm(20),
    CoP_Y = rnorm(20)
  )

  result <- SD_CoP_ComputeR(data = df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")

  # Should be sorted by participant ID alphabetically
  expect_equal(result$participant_id, c("A", "B"))
})
