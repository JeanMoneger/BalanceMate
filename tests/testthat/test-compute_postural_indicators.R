# test-compute_postural_indicators.R
library(testthat)

test_that("compute_postural_indicators validates indicators", {
  df <- data.frame(ID = rep("A", 5), CoP_X = rnorm(5), CoP_Y = rnorm(5))

  expect_error(
    compute_postural_indicators(
      data = df,
      CoPX_col = "CoP_X",
      CoPY_col = "CoP_Y",
      ID = "ID",
      indicators = c("InvalidIndicator")
    ),
    "Invalid indicator\\(s\\): InvalidIndicator"
  )
})

test_that("compute_postural_indicators computes EllipseArea correctly", {
  df <- data.frame(ID = rep("A", each = 500), CoP_X = rnorm(500), CoP_Y = rnorm(500))

  result <- compute_postural_indicators(
    data = df,
    CoPX_col = "CoP_X",
    CoPY_col = "CoP_Y",
    ID = "ID",
    indicators = c("EllipseArea")
  )

  expect_true("EllipseArea" %in% names(result))
  expect_equal(result$EllipseArea, BalanceMate::compute_ellipse_area(df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")$ellipse_area)
})

test_that("compute_postural_indicators computes SwayPathLength correctly", {
  df <- data.frame(ID = rep("A",5), CoP_X = rnorm(5), CoP_Y = rnorm(5))

  result <- compute_postural_indicators(
    data = df,
    CoPX_col = "CoP_X",
    CoPY_col = "CoP_Y",
    ID = "ID",
    indicators = c("SwayPathLength")
  )

  expect_true("SwayPathLength" %in% names(result))
  expect_equal(result$SwayPathLength, BalanceMate::SPL_ComputeR(df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")$sway_path_length)
})

test_that("compute_postural_indicators computes SD_CoP_X and SD_CoP_Y correctly", {
  df <- data.frame(ID = rep("A",5), CoP_X = rnorm(5), CoP_Y = rnorm(5))

  result <- compute_postural_indicators(
    data = df,
    CoPX_col = "CoP_X",
    CoPY_col = "CoP_Y",
    ID = "ID",
    indicators = c("SD_CoP_X", "SD_CoP_Y")
  )

  expect_true("SD_CoP_X" %in% names(result))
  expect_true("SD_CoP_Y" %in% names(result))
  expect_equal(result$SD_CoP_X, BalanceMate::SD_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$SD_CoPX)
  expect_equal(result$SD_CoP_Y, BalanceMate::SD_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$SD_CoPY)
})

test_that("compute_postural_indicators computes CoP_X and CoP_Y means correctly", {
  df <- data.frame(ID = rep("A",5), CoP_X = rnorm(5), CoP_Y = rnorm(5))

  result <- compute_postural_indicators(
    data = df,
    CoPX_col = "CoP_X",
    CoPY_col = "CoP_Y",
    ID = "ID",
    indicators = c("CoP_X", "CoP_Y")
  )

  expect_true("CoP_X" %in% names(result))
  expect_true("CoP_Y" %in% names(result))
  expect_equal(result$CoP_X, BalanceMate::Mean_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$mean_CoPX)
  expect_equal(result$CoP_Y, BalanceMate::Mean_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$mean_CoPY)
})

test_that("compute_postural_indicators integrates multiple indicators", {
  df <- data.frame(ID = rep(c("A", "B"),each = 5), CoP_X = rnorm(10), CoP_Y = rnorm(10))

  result <- compute_postural_indicators(
    data = df,
    CoPX_col = "CoP_X",
    CoPY_col = "CoP_Y",
    ID = "ID",
    indicators = c("EllipseArea", "SwayPathLength", "SD_CoP_X", "SD_CoP_Y", "CoP_X", "CoP_Y")
  )

  expect_true(all(c("EllipseArea", "SwayPathLength", "SD_CoP_X", "SD_CoP_Y", "CoP_X", "CoP_Y") %in% names(result)))
  expect_equal(result$EllipseArea, BalanceMate::compute_ellipse_area(df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")$ellipse_area)
  expect_equal(result$SwayPathLength, BalanceMate::SPL_ComputeR(df, CoPX_col = "CoP_X", CoPY_col = "CoP_Y", ID = "ID")$sway_path_length)
  expect_equal(result$SD_CoP_X, BalanceMate::SD_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$SD_CoPX)
  expect_equal(result$SD_CoP_Y, BalanceMate::SD_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$SD_CoPY)
  expect_equal(result$CoP_X, BalanceMate::Mean_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$mean_CoPX)
  expect_equal(result$CoP_Y, BalanceMate::Mean_CoP_ComputeR(df, "CoP_X", "CoP_Y", "ID")$mean_CoPY)

  })

test_that("compute_postural_indicators respects epochs when provided", {
  df <- data.frame(
    ID = rep(1, 100),
    Time = seq(0, 9.9, by = 0.1),
    CoP_X = rnorm(100),
    CoP_Y = rnorm(100)
  )

  result <- compute_postural_indicators(
    data = df,
    CoPX_col = "CoP_X",
    CoPY_col = "CoP_Y",
    ID = "ID",
    time_col = "Time",
    epoch_length = 2,
    indicators = c("SwayPathLength")
  )

  expect_true("epoch" %in% names(result))
  expect_equal(length(unique(result$epoch)), 5)  # 10 seconds / 2-second epochs
})
