# test-Epoch_SliceR.R
library(testthat)

test_that("Epoch_SliceR handles basic case with full epochs and expected sample count", {
  # Create a simple data set with 2 participants, 2-second epochs, 100 Hz, 10-second sessions
  # Total samples per participant: 100 Hz * 10 s = 1000 samples
  # Epoch length = 2 s => 2 * 100 = 200 samples per epoch
  # => Each participant has 5 full epochs
  set.seed(123)
  sample_rate <- 100
  session_duration <- 10
  epoch_length <- 2
  samples_per_participant <- sample_rate * session_duration
  participant_ids <- c("P1", "P2")

  df <- data.frame(
    file_name = rep(participant_ids, each = samples_per_participant),
    CoP_X = rnorm(length(participant_ids) * samples_per_participant),
    CoP_Y = rnorm(length(participant_ids) * samples_per_participant),
    OtherVar = rep(1:1000, times = length(participant_ids)) # Some extra variable
  )

  result <- Epoch_SliceR(
    df = df,
    ID = "file_name",
    columns_to_synthesize = c("CoP_X", "CoP_Y"),
    epoch_length = epoch_length,
    sample_rate = sample_rate,
    session_duration = session_duration
  )

  # Check structure
  expect_true("ID" %in% names(result))
  expect_true("Epoch" %in% names(result))
  expect_true("Mean_CoP_X" %in% names(result))
  expect_true("SD_CoP_X" %in% names(result))
  expect_true("Mean_CoP_Y" %in% names(result))
  expect_true("SD_CoP_Y" %in% names(result))

  # Each participant should have 5 epochs (10 s / 2 s per epoch = 5 epochs)
  expect_equal(nrow(result[result$ID == "P1", ]), 5)
  expect_equal(nrow(result[result$ID == "P2", ]), 5)

  # OtherVar should be present and contain the first value of that epoch window
  expect_true("OtherVar" %in% names(result))
  # First epoch of P1 should have OtherVar = the first sample of that participant's data
  first_P1_val <- df[df$file_name == "P1", "OtherVar"][1]
  expect_equal(result$OtherVar[result$ID == "P1" & result$Epoch == 1], first_P1_val)
})

test_that("Epoch_SliceR warns if participant does not have expected number of samples", {
  # One participant has fewer samples than expected
  sample_rate <- 50
  session_duration <- 4
  epoch_length <- 1
  samples_per_participant <- sample_rate * session_duration

  # P1: correct number of samples
  # P2: fewer samples
  df <- data.frame(
    file_name = c(rep("P1", samples_per_participant),
                  rep("P2", samples_per_participant - 10)),
    CoP_X = rnorm(samples_per_participant * 2 - 10),
    CoP_Y = rnorm(samples_per_participant * 2 - 10)
  )
  warnings <- c()
  withCallingHandlers(
    Epoch_SliceR(
      df = df,
      ID = "file_name",
      columns_to_synthesize = c("CoP_X", "CoP_Y"),
      epoch_length = epoch_length,
      sample_rate = sample_rate,
      session_duration = session_duration
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(
    any(grepl("Participant 'P2' has an incomplete final epoch with only 40 samples \\(expected 50 samples per epoch\\)\\.", warnings))
  )

})

test_that("Epoch_SliceR warns if final epoch is incomplete", {
  sample_rate <- 100
  session_duration <- 5
  epoch_length <- 2
  samples_per_participant <- sample_rate * session_duration

  # We'll remove some samples from the end to force an incomplete epoch
  df <- data.frame(
    file_name = rep("P1", samples_per_participant - 50), # missing some samples
    CoP_X = rnorm(samples_per_participant - 50),
    CoP_Y = rnorm(samples_per_participant - 50)
  )

  warnings <- c()
  withCallingHandlers(
    Epoch_SliceR(
      df = df,
      ID = "file_name",
      columns_to_synthesize = c("CoP_X", "CoP_Y"),
      epoch_length = epoch_length,
      sample_rate = sample_rate,
      session_duration = session_duration
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(
    any(grepl("Participant 'P1' has an incomplete final epoch with only 50 samples \\(expected 200 samples per epoch\\)\\.", warnings))
  )
})

test_that("Epoch_SliceR computes mean and standard deviation correctly", {
  # Create a small dataset where we know the values
  sample_rate <- 10
  session_duration <- 2
  epoch_length <- 1
  # Each participant: 10 samples per second * 2 s = 20 samples total
  # Epoch length = 1 s => 10 samples per epoch
  df <- data.frame(
    file_name = rep("P1", 20),
    CoP_X = 1:20,     # Increasing sequence
    CoP_Y = rep(5, 20) # Constant
  )

  result <- Epoch_SliceR(
    df = df,
    ID = "file_name",
    columns_to_synthesize = c("CoP_X", "CoP_Y"),
    epoch_length = epoch_length,
    sample_rate = sample_rate,
    session_duration = session_duration
  )

  # We have 2 epochs:
  # Epoch 1: CoP_X = 1:10, mean=5.5, sd=3.027...; CoP_Y = all 5, mean=5, sd=0
  # Epoch 2: CoP_X = 11:20, mean=15.5, sd=3.027...; CoP_Y = all 5, mean=5, sd=0
  expect_equal(result$Mean_CoP_X[1], mean(1:10))
  expect_equal(result$Mean_CoP_X[2], mean(11:20))
  expect_equal(result$SD_CoP_X[1], sd(1:10))
  expect_equal(result$SD_CoP_X[2], sd(11:20))

  expect_equal(result$Mean_CoP_Y, c(5, 5))
  expect_equal(result$SD_CoP_Y, c(0, 0))
})

test_that("Epoch_SliceR merges back all participants data", {
  sample_rate <- 10
  session_duration <- 2
  epoch_length <- 1

  df <- data.frame(
    file_name = c(rep("A", 20), rep("B", 20)),
    CoP_X = rnorm(40),
    CoP_Y = rnorm(40)
  )

  result <- Epoch_SliceR(
    df = df,
    ID = "file_name",
    columns_to_synthesize = c("CoP_X", "CoP_Y"),
    epoch_length = epoch_length,
    sample_rate = sample_rate,
    session_duration = session_duration
  )

  # Each participant should have the same number of epochs: 2 seconds / 1 second epoch = 2 epochs
  expect_equal(as.numeric(table(result$ID)), c(2, 2))
})

test_that("Epoch_SliceR handles extra columns gracefully", {
  sample_rate <- 10
  session_duration <- 2
  epoch_length <- 1

  df <- data.frame(
    file_name = rep("P1", 20),
    CoP_X = rnorm(20),
    CoP_Y = rnorm(20),
    Extra = 1:20  # This should be the first value in each epoch in result
  )

  result <- Epoch_SliceR(
    df = df,
    ID = "file_name",
    columns_to_synthesize = c("CoP_X", "CoP_Y"),
    epoch_length = epoch_length,
    sample_rate = sample_rate,
    session_duration = session_duration
  )

  # Epoch 1: first value of Extra = 1
  # Epoch 2: first value of Extra = 11
  expect_equal(result$Extra[1], 1)
  expect_equal(result$Extra[2], 11)
})

test_that("Epoch_SliceR handles no columns_to_synthesize", {
  # If no columns are given to synthesize, we only return participant, epoch, and other columns
  sample_rate <- 10
  session_duration <- 2
  epoch_length <- 1

  df <- data.frame(
    file_name = rep("P1", 20),
    Var1 = rnorm(20),
    Var2 = rnorm(20)
  )

  result <- Epoch_SliceR(
    df = df,
    ID = "file_name",
    columns_to_synthesize = character(0), # no columns to synthesize
    epoch_length = epoch_length,
    sample_rate = sample_rate,
    session_duration = session_duration
  )

  # Should not have Mean_ or SD_ columns
  expect_false(any(grepl("^Mean_", names(result))))
  expect_false(any(grepl("^SD_", names(result))))

  # Should have returned Var1 and Var2 as the first value of each epoch
  expect_true("Var1" %in% names(result))
  expect_true("Var2" %in% names(result))
})

