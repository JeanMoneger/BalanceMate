# test-Butterworth_it.R
library(testthat)

test_that("Butterworth_it throws an error for non-data.frame input", {
  non_df_input <- matrix(1:10, ncol = 2)
  expect_error(
    Butterworth_it(
      Data = non_df_input,
      cutoff_freq = 10,
      filter_order = 2,
      sampling_rate = 100,
      type = "low",
      Colname = "V1"
    ),
    "Data must be a data frame."
  )
})

test_that("Butterworth_it throws an error if Colname not in Data", {
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  expect_error(
    Butterworth_it(
      Data = df,
      cutoff_freq = 10,
      filter_order = 2,
      sampling_rate = 100,
      type = "low",
      Colname = "z"
    ),
    "The following columns are not in Data: z"
  )
})

test_that("Butterworth_it throws an error if type is invalid", {
  df <- data.frame(x = rnorm(100))
  expect_error(
    Butterworth_it(
      Data = df,
      cutoff_freq = 10,
      filter_order = 2,
      sampling_rate = 100,
      type = "bandpass",  # invalid
      Colname = "x"
    ),
    "Type must be either 'low' or 'high'."
  )
})

test_that("Butterworth_it adds filtered columns with '_filtered' suffix", {
  set.seed(123)
  df <- data.frame(x = rnorm(100))
  out <- Butterworth_it(
    Data = df,
    cutoff_freq = 10,
    filter_order = 2,
    sampling_rate = 100,
    type = "low",
    Colname = "x"
  )
  expect_true("x_filtered" %in% names(out))
})

test_that("Butterworth_it does not overwrite original columns", {
  set.seed(123)
  df <- data.frame(x = rnorm(100))
  out <- Butterworth_it(
    Data = df,
    cutoff_freq = 10,
    filter_order = 2,
    sampling_rate = 100,
    type = "low",
    Colname = "x"
  )
  # Original 'x' should remain unchanged
  expect_equal(out$x, df$x)
})

test_that("Butterworth_it filters multiple columns correctly", {
  set.seed(123)
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  out <- Butterworth_it(
    Data = df,
    cutoff_freq = 5,
    filter_order = 4,
    sampling_rate = 100,
    type = "low",
    Colname = c("x", "y")
  )
  # Check new columns
  expect_true("x_filtered" %in% names(out))
  expect_true("y_filtered" %in% names(out))

  # Check that filtered values differ from the originals
  expect_false(isTRUE(all.equal(out$x_filtered, out$x)))
  expect_false(isTRUE(all.equal(out$y_filtered, out$y)))
})

test_that("Butterworth_it works with a known signal", {
  # Create a synthetic signal: a combination of a low-frequency and a high-frequency sine wave
  # The low-pass filter should remove most of the high-frequency component
  set.seed(123)
  sampling_rate <- 1000
  t <- seq(0, 1, length.out = sampling_rate)
  low_freq_signal <- sin(2 * pi * 5 * t)     # 5 Hz component
  high_freq_signal <- sin(2 * pi * 50 * t)   # 50 Hz component
  combined_signal <- low_freq_signal + high_freq_signal

  df <- data.frame(signal = combined_signal)

  # Apply a low-pass filter with cutoff at 10 Hz
  out <- Butterworth_it(
    Data = df,
    cutoff_freq = 10,
    filter_order = 4,
    sampling_rate = sampling_rate,
    type = "low",
    Colname = "signal"
  )

  # Expect that the filtered signal is closer to the low_freq_signal than combined_signal is
  original_error <- mean((combined_signal - low_freq_signal)^2)
  filtered_error <- mean((out$signal_filtered - low_freq_signal)^2)

  # The filtered signal should have less error from the low_freq_signal than the original
  expect_lt(filtered_error, original_error)
})

test_that("Butterworth_it can perform a high-pass filter", {
  # Check that a high-pass filter with a relatively high cutoff retains high frequencies and discards low frequencies
  sampling_rate <- 1000
  t <- seq(0, 1, length.out = sampling_rate)
  low_freq_signal <- sin(2 * pi * 5 * t)
  high_freq_signal <- sin(2 * pi * 50 * t)
  combined_signal <- low_freq_signal + high_freq_signal

  df <- data.frame(signal = combined_signal)

  # High-pass filter at 30 Hz
  out <- Butterworth_it(
    Data = df,
    cutoff_freq = 30,
    filter_order = 4,
    sampling_rate = sampling_rate,
    type = "high",
    Colname = "signal"
  )

  # Expect that the filtered signal is closer to the high_freq_signal than the original combined
  original_error <- mean((combined_signal - high_freq_signal)^2)
  filtered_error <- mean((out$signal_filtered - high_freq_signal)^2)

  expect_lt(filtered_error, original_error)
})
