# Test CoPX_ComputeR function
test_that("CoPX_ComputeR works correctly", {
  # Create a small vector
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, 5)
  z <- x/y

  # Expect the mean of the vector to be 3
  expect_equal(CoPX_ComputeR(x,y), z)

  # try something else
  Test<-Merge_PosData(directory_path = "inst/extdata", SampleRate = 100, SessionDuration = 331)
  # Expect the CoP computation of CoP_Computers to be equal to the computation performed in Merger function
  expect_equal(CoPX_ComputeR(Test$My,Test$Fz), Test$CoP_X)

})
