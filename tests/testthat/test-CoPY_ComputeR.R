# test-CoPY_ComputeR.R
library(testthat)

test_that("CoPY_ComputeR computes CoP-Y correctly", {
  # Simple case with known values
  Mx <- c(1, 2, 3)
  Fz <- c(10, 20, 30)

  result <- CoPY_ComputeR(Mx, Fz)
  expected <- c(10, 10, 10)  # Mx / Fz * 100
  expect_equal(result, expected)
})

test_that("CoPY_ComputeR handles vectorized inputs", {
  Mx <- 1:5
  Fz <- rep(10, 5)

  result <- CoPY_ComputeR(Mx, Fz)
  expected <- (Mx / Fz) * 100
  expect_equal(result, expected)
})

test_that("CoPY_ComputeR raises error when Fz contains zeros", {
  Mx <- c(1, 2, 3)
  Fz <- c(10, 0, 30)

  expect_error(
    CoPY_ComputeR(Mx, Fz),
    "Fz cannot be zero to compute CoP Y."
  )
})

test_that("CoPY_ComputeR handles negative values in Mx and Fz correctly", {
  Mx <- c(-1, 2, -3)
  Fz <- c(-10, -20, -30)

  result <- CoPY_ComputeR(Mx, Fz)
  expected <- c((-1/-10)*100, (2/-20)*100, (-3/-30)*100)  # Mx / Fz * 100
  expect_equal(result, expected)
})

test_that("CoPY_ComputeR handles large numeric values", {
  Mx <- c(1e6, 2e6, 3e6)
  Fz <- c(1e5, 2e5, 3e5)

  result <- CoPY_ComputeR(Mx, Fz)
  expected <- (Mx / Fz) * 100
  expect_equal(result, expected)
})

test_that("CoPY_ComputeR returns numeric output", {
  Mx <- c(1, 2, 3)
  Fz <- c(10, 20, 30)

  result <- CoPY_ComputeR(Mx, Fz)
  expect_true(is.numeric(result))
})

test_that("CoPY_ComputeR raises error for non-numeric inputs", {
  Mx <- c("a", "b", "c")  # Invalid input
  Fz <- c(10, 20, 30)

  expect_error(
    CoPY_ComputeR(Mx, Fz),
    "non-numeric argument to binary operator"
  )

  Mx <- c(1, 2, 3)
  Fz <- c("x", "y", "z")  # Invalid input

  expect_error(
    CoPY_ComputeR(Mx, Fz),
    "non-numeric argument to binary operator"
  )
})

test_that("CoPY_ComputeR handles edge case with single-element vectors", {
  Mx <- 10
  Fz <- 100

  result <- CoPY_ComputeR(Mx, Fz)
  expected <- (Mx / Fz) * 100
  expect_equal(result, expected)
})

test_that("CoPY_ComputeR raises error for mismatched vector lengths", {
  Mx <- c(1, 2, 3)
  Fz <- c(10, 20)  # Different length

  expect_error(
    CoPY_ComputeR(Mx, Fz),
    "Vectors 'Mx' and 'Fz' must be of the same length."
  )
})
