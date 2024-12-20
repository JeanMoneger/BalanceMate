# test-CoPX_ComputeR.R
library(testthat)

test_that("CoPX_ComputeR computes CoP-X correctly", {
  # Simple case with known values
  My <- c(1, 2, 3)
  Fz <- c(10, 20, 30)

  result <- CoPX_ComputeR(My, Fz)
  expected <- c(-10, -10, -10)  # -My / Fz * 100
  expect_equal(result, expected)
})

test_that("CoPX_ComputeR handles vectorized inputs", {
  My <- 1:5
  Fz <- rep(10, 5)

  result <- CoPX_ComputeR(My, Fz)
  expected <- (-My / Fz) * 100
  expect_equal(result, expected)
})

test_that("CoPX_ComputeR raises error when Fz contains zeros", {
  My <- c(1, 2, 3)
  Fz <- c(10, 0, 30)

  expect_error(
    CoPX_ComputeR(My, Fz),
    "Fz cannot be zero to compute CoP X."
  )
})

test_that("CoPX_ComputeR handles negative values in My and Fz correctly", {
  My <- c(-1, 2, -3)
  Fz <- c(-10, -20, -30)

  result <- CoPX_ComputeR(My, Fz)
  expected <- c(-10,  10, -10)  # -My / Fz * 100
  expect_equal(result, expected)
})

test_that("CoPX_ComputeR handles large numeric values", {
  My <- c(1e6, 2e6, 3e6)
  Fz <- c(1e5, 2e5, 3e5)

  result <- CoPX_ComputeR(My, Fz)
  expected <- (-My / Fz) * 100
  expect_equal(result, expected)
})

test_that("CoPX_ComputeR returns numeric output", {
  My <- c(1, 2, 3)
  Fz <- c(10, 20, 30)

  result <- CoPX_ComputeR(My, Fz)
  expect_true(is.numeric(result))
})

test_that("CoPX_ComputeR raises error for non-numeric inputs", {
  My <- c("a", "b", "c")  # Invalid input
  Fz <- c(10, 20, 30)

  expect_error(
    CoPX_ComputeR(My, Fz),
    "invalid argument to unary operator"
  )

  My <- c(1, 2, 3)
  Fz <- c("x", "y", "z")  # Invalid input

  expect_error(
    CoPX_ComputeR(My, Fz),
    "non-numeric argument to binary operator"
  )
})

test_that("CoPX_ComputeR handles edge case with single-element vectors", {
  My <- 10
  Fz <- 100

  result <- CoPX_ComputeR(My, Fz)
  expected <- (-My / Fz) * 100
  expect_equal(result, expected)
})

test_that("CoPX_ComputeR raises error for mismatched vector lengths", {
  My <- c(1, 2, 3)
  Fz <- c(10, 20)  # Different length

  expect_error(
    CoPX_ComputeR(My, Fz),
    "Vectors 'My' and 'Fz' must be of the same length."
  )
})
