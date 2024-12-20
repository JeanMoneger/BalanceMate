# test-compute_ellipse_area.R
library(testthat)

test_that("compute_ellipse_area checks for required columns", {
  df <- data.frame(
    ID = 1:10,
    CoP_X = rnorm(10),
    CoP_Y = rnorm(10),
    Time = seq(0, 0.9, by = 0.1)
  )

  # Missing CoPX_col
  expect_error(
    compute_ellipse_area(df, "Wrong_CoP_X", "CoP_Y", "ID"),
    "Column Wrong_CoP_X not found in the data frame"
  )

  # Missing CoPY_col
  expect_error(
    compute_ellipse_area(df, "CoP_X", "Wrong_CoP_Y", "ID"),
    "Column Wrong_CoP_Y not found in the data frame"
  )

  # Missing ID
  expect_error(
    compute_ellipse_area(df, "CoP_X", "CoP_Y", "Wrong_ID"),
    "Column Wrong_ID not found in the data frame"
  )
})

test_that("compute_ellipse_area works without epochs", {
  set.seed(123)
  df <- data.frame(
    ID = rep(c("A", "B"), each = 50),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  result <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID")
  expect_true("ellipse_area" %in% names(result))
  expect_true("participant_id" %in% names(result))
  # Expect one row per participant
  expect_equal(nrow(result), 2)
  expect_equal(sort(unique(result$participant_id)), c("A", "B"))
})

test_that("compute_ellipse_area works with epochs", {
  set.seed(123)
  df <- data.frame(
    ID = rep("A", 100),
    Time = seq(0.1, 10, by = 0.1),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  # Epoch length = 2 seconds => 5 epochs for a 10-second recording (0 to 9.9)
  result <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID", time_col = "Time", epoch_length = 2)
  expect_true("ellipse_area" %in% names(result))
  expect_true("epoch" %in% names(result))
  expect_equal(length(unique(result$epoch)), 5)
  expect_equal(nrow(result), 5)
})

test_that("compute_ellipse_area warns if epochs are unbalanced", {
  set.seed(123)
  df <- data.frame(
    ID = rep("A", 105),        # 10.4 seconds total (approx)
    Time = seq(0, 10.4, by = 0.1),
    CoP_X = rnorm(105),
    CoP_Y = rnorm(105)
  )

  expect_warning(
    compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID", time_col = "Time", epoch_length = 2),
    "Unbalanced epochs detected"
  )
})

test_that("compute_ellipse_area returns NA if insufficient data in a group", {
  df <- data.frame(
    ID = c(rep("A", 2), rep("B", 1)),  # Participant B has only 1 data point
    CoP_X = c(rnorm(2), 1),
    CoP_Y = c(rnorm(2), 2)
  )

  result <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID")
  # Participant B's ellipse_area should be NA due to insufficient data (1 row)
  expect_true(is.na(result$ellipse_area[result$participant_id == "B"]))
})

test_that("compute_ellipse_area returns NA for singular covariance matrices", {
  # All CoP_X and CoP_Y are the same for participant B, covariance is singular
  df <- data.frame(
    ID = c(rep("A", 10), rep("B", 10)),
    CoP_X = c(rnorm(10), rep(1, 10)),
    CoP_Y = c(rnorm(10), rep(1, 10))
  )

  result <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID")
  # Participant B should return NA
  expect_true(is.na(result$ellipse_area[result$participant_id == "B"]))
})

test_that("compute_ellipse_area computation correctness with known covariance", {
  # Known scenario: If CoP_X and CoP_Y are drawn from N(0,1), large sample ~ covariance ~ I.
  # Ellipse area formula: pi * qchisq(confint, df=2) * sqrt(lambda1 * lambda2)
  # If covariance ~ I, lambda1 ~ 1, lambda2 ~ 1 => area ~ pi * qchisq(0.95, 2)
  # qchisq(0.95,2) ~ 5.9915. So expected area ~ pi * 5.9915 ~ 18.82 approx.
  # We'll use a large sample to get close to identity covariance.

  set.seed(123)
  df <- data.frame(
    ID = "A",
    CoP_X = rnorm(10000), # Large sample from N(0,1)
    CoP_Y = rnorm(10000)
  )

  result <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID", confint = 0.95)
  # Check if the result is close to expected
  expect_true(abs(result$ellipse_area - (pi * qchisq(0.95, 2))) < 1) # Within 1 cm² tolerance
})

test_that("compute_ellipse_area changes with different confint values", {
  set.seed(123)
  df <- data.frame(
    ID = "A",
    CoP_X = rnorm(1000),
    CoP_Y = rnorm(1000)
  )

  result_95 <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID", confint = 0.95)
  result_80 <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID", confint = 0.80)

  # Since qchisq(0.80, 2) < qchisq(0.95, 2), area should be smaller
  expect_true(result_80$ellipse_area < result_95$ellipse_area)
})

test_that("compute_ellipse_area result ordering is correct", {
  df <- data.frame(
    ID = rep(c("B", "A"), each = 20), # Intentionally out of order participants
    Time = rep(seq(0,1.9,0.1), 2),
    CoP_X = rnorm(40),
    CoP_Y = rnorm(40)
  )

  result <- compute_ellipse_area(df, "CoP_X", "CoP_Y", "ID", time_col = "Time", epoch_length = 1)
  # Participants should be in alphabetical order (A first, then B)
  expect_identical(unique(result$participant_id), c("A", "B"))
  # Within each participant, epochs should be sorted
  for (pid in unique(result$participant_id)) {
    expect_identical(result$epoch[result$participant_id == pid], sort(result$epoch[result$participant_id == pid]))
  }
})

