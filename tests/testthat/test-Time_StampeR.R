# test-Time_StampeR.R
library(testthat)

test_that("Time_StampeR adds a Time column and handles single participant", {
  sample_rate <- 10
  protocol_duration <- 5
  n <- sample_rate * protocol_duration
  df <- data.frame(
    ID = rep("P1", n),
    CoP_X = rnorm(n),
    CoP_Y = rnorm(n)
  )

  result <- Time_StampeR(
    df = df,
    id_col = "ID",
    sample_rate = sample_rate,
    protocol_duration = protocol_duration
  )

  expect_true("Time" %in% names(result))
  expect_equal(length(unique(result$ID)), 1)
  # Time should start at 0 and increment by 1/sample_rate
  expect_equal(min(result$Time), 0)
  expect_equal(max(result$Time), protocol_duration - (1/sample_rate))
})

test_that("Time_StampeR warns if participant has unexpected number of rows", {
  sample_rate <- 10
  protocol_duration <- 5
  expected_rows <- sample_rate * protocol_duration

  # Give participant fewer rows
  df <- data.frame(
    ID = rep("P1", expected_rows - 2),
    CoP_X = rnorm(expected_rows - 2),
    CoP_Y = rnorm(expected_rows - 2)
  )

  expect_warning(
    Time_StampeR(
      df = df,
      id_col = "ID",
      sample_rate = sample_rate,
      protocol_duration = protocol_duration
    ),
    "has \\d+ rows, expected \\d+"
  )
})

test_that("Time_StampeR handles multiple participants", {
  sample_rate <- 5
  protocol_duration <- 2
  n <- sample_rate * protocol_duration

  df <- data.frame(
    ID = c(rep("A", n), rep("B", n)),
    CoP_X = rnorm(2 * n),
    CoP_Y = rnorm(2 * n)
  )

  result <- Time_StampeR(
    df = df,
    id_col = "ID",
    sample_rate = sample_rate,
    protocol_duration = protocol_duration
  )

  expect_true("Time" %in% names(result))
  # Check each participant's time
  for (pid in c("A", "B")) {
    subset_data <- result[result$ID == pid, ]
    expect_equal(nrow(subset_data), n)
    expect_equal(min(subset_data$Time), 0)
    expect_equal(max(subset_data$Time), protocol_duration - (1/sample_rate))
  }
})

test_that("Time_StampeR assigns periods correctly with valid cuts and period_names", {
  sample_rate <- 10
  protocol_duration <- 10
  n <- sample_rate * protocol_duration
  df <- data.frame(
    ID = rep("P1", n),
    CoP_X = rnorm(n),
    CoP_Y = rnorm(n)
  )

  # Suppose we have a 10s protocol and we cut at 3s and 7s:
  # Intervals: 0-3, 3-7, 7-10
  cuts <- c(3, 7)
  periods <- c("Phase1", "Phase2", "Phase3")

  result <- Time_StampeR(
    df = df,
    id_col = "ID",
    sample_rate = sample_rate,
    protocol_duration = protocol_duration,
    cuts = cuts,
    period_names = periods
  )

  expect_true("Period_Name" %in% names(result))

  # Check intervals:
  # Phase1: Time < 3
  expect_true(all(result$Period_Name[result$Time < 3] == "Phase1"))
  # Phase2: 3 <= Time < 7
  expect_true(all(result$Period_Name[result$Time >= 3 & result$Time < 7] == "Phase2"))
  # Phase3: 7 <= Time < 10
  expect_true(all(result$Period_Name[result$Time >= 7] == "Phase3"))
})

test_that("Time_StampeR errors if period_names not matching the cuts", {
  df <- data.frame(
    ID = rep("P1", 100),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  cuts <- c(30, 60)
  # We need one more label than cuts: here we have 2 cuts, so 3 labels needed.
  period_names <- c("Part1", "Part2") # only 2 labels, should cause error

  expect_error(
    Time_StampeR(
      df = df,
      id_col = "ID",
      sample_rate = 10,
      protocol_duration = 10,
      cuts = cuts,
      period_names = period_names
    ),
    "The number of period names must be one more than the number of cut points"
  )
})

test_that("Time_StampeR warns if some time points not assigned a period", {
  df <- data.frame(
    ID = rep("P1", 100),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  # Suppose cuts and period_names don't cover the whole protocol_duration
  cuts <- c(2, 5)  # protocol_duration = 10 not given fully
  # Intervals: 0-2, 2-5, 5-10

  # This will first error due to mismatch in periods, so let's fix that.
  # Correct number of period names for two cuts = three intervals
  period_names <- c("Start", "Middle", "???")

  # This covers all intervals 0-2, 2-5, 5-10, so no warning expected here.
  # To produce a warning, let's deliberately not cover full duration.
  # For example, if we say protocol_duration = 7 (instead of 10)
  # Then intervals are: 0-2, 2-5, 5-7 but we have a 10-second data actually
  # Actually, the function uses protocol_duration to define intervals.
  # If we mismatch data rows and protocol_duration:
  # Let's set protocol_duration = 7, but we have 100 data points (10s at 10Hz)
  # The last 3 seconds (Time >=7) won't be assigned.
  warnings <- c()
  withCallingHandlers(
    Time_StampeR(
      df = df,
      id_col = "ID",
      sample_rate = 10,         # This implies total duration covered by data = 10s
      protocol_duration = 7,    # Only define periods for first 7s
      cuts = cuts,
      period_names = period_names
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Assert that the second warning is present
  expect_true(
    any(grepl("Some time points were not assigned to any period\\.", warnings))
  )


})

