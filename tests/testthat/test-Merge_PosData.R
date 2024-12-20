# test-Merge_PosData.R
library(testthat)

test_that("Merge_PosData stops if no .txt files are found", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)  # Ensure working directory is reset

  expect_error(
    Merge_PosData(directory_path = temp_dir, SampleRate = 100),
    "No .txt files found in the specified directory."
  )
})



test_that("Merge_PosData checks for valid SampleRate and SessionDuration", {
  # Create a temporary directory
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  write.table(data.frame(Fx = 1, Fy = 1, Fz = 1, Mx = 1, My = 1, Mz = 1),
              file = "test.txt", row.names = FALSE, col.names = FALSE, sep = ",")

  on.exit(setwd(old_wd), add = TRUE)  # Ensure working directory is reset

    expect_error(Merge_PosData(directory_path = temp_dir, SampleRate = -1),
                 "Invalid input for SampleRate. Please enter a positive number.")

    expect_error(Merge_PosData(directory_path = temp_dir, SampleRate = NA),
                 "Invalid input for SampleRate. Please enter a positive number.")

    expect_error(Merge_PosData(directory_path = temp_dir, SampleRate = 100, SessionDuration = -10),
                 "Invalid input for SessionDuration. Please enter a positive number.")

    expect_error(Merge_PosData(directory_path = temp_dir, SampleRate = 100, SessionDuration = NA),
                 "Invalid input for SessionDuration. Please enter a positive number.")
    unlink("test.txt")

  })


test_that("Merge_PosData merges files correctly without SessionDuration", {
    # Create two test files with known data
    # Suppose SampleRate = 100, each file has 200 rows = 2 seconds
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

      set.seed(123)
    data1 <- data.frame(
      Fx = rnorm(200),
      Fy = rnorm(200),
      Fz = rnorm(200, mean = -100), # non-zero to avoid division by zero
      Mx = rnorm(200),
      My = rnorm(200),
      Mz = rnorm(200)
    )
    data2 <- data.frame(
      Fx = rnorm(200),
      Fy = rnorm(200),
      Fz = rnorm(200, mean = -100),
      Mx = rnorm(200),
      My = rnorm(200),
      Mz = rnorm(200)
    )

    write.table(data1, file = "file1.txt", row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(data2, file = "file2.txt", row.names = FALSE, col.names = FALSE, sep = ",")

    result <- Merge_PosData(directory_path = temp_dir, SampleRate = 100)
    expect_equal(nrow(result), 400) # 200 from each file
    expect_equal(ncol(result), 10) # Fx,Fy,Fz,Mx,My,Mz,Time,file_name,CoP_X,CoP_Y

    # Check column names
    expect_true(all(c("Fx", "Fy", "Fz", "Mx", "My", "Mz", "Time", "file_name", "CoP_X", "CoP_Y") %in% names(result)))

    # Check Time increments correctly
    # Time should start at 1/100 and go to 2 seconds for the first file,
    # then again start at 1/100 for the second file. Actually, since the files are merged,
    # time columns are not reset per file but computed per file individually.
    # After merging, we expect that Time is whatever was computed per file. Because we merge with rbind,
    # the second file rows come after the first file rows.
    # We'll just check the range of Time for correctness in each file's block:
    file1_time <- result$Time[1:200]
    file2_time <- result$Time[201:400]
    expect_equal(min(file1_time), 1/100)
    expect_equal(max(file1_time), 2)    # since duration = 200/100 = 2s
    expect_equal(min(file2_time), 1/100)
    expect_equal(max(file2_time), 2)

    # Check CoP_X and CoP_Y calculations
    # CoP_X = (-My / Fz)*100
    # CoP_Y = (Mx / Fz)*100
    # We'll just check a few random rows
    idx <- sample(1:400, 5)
    expect_equal(result$CoP_X[idx], (-result$My[idx] / result$Fz[idx]) * 100)
    expect_equal(result$CoP_Y[idx], (result$Mx[idx] / result$Fz[idx]) * 100)
    unlink(c("file1.txt", "file2.txt"))
  })


test_that("Merge_PosData checks SessionDuration and raises errors if mismatched rows", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  SampleRate <- 100
    SessionDuration <- 2
    # Expected rows = 100 * 2 = 200

    # First file has correct rows
    data1 <- data.frame(
      Fx = rnorm(200),
      Fy = rnorm(200),
      Fz = rnorm(200, mean = -100),
      Mx = rnorm(200),
      My = rnorm(200),
      Mz = rnorm(200)
    )
    # Second file has fewer rows
    data2 <- data.frame(
      Fx = rnorm(100),
      Fy = rnorm(100),
      Fz = rnorm(100, mean = -100),
      Mx = rnorm(100),
      My = rnorm(100),
      Mz = rnorm(100)
    )

    write.table(data1, file = "correct.txt", row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(data2, file = "short.txt", row.names = FALSE, col.names = FALSE, sep = ",")

    # Expect an error because short.txt doesn't have the expected rows
    expect_error(
      Merge_PosData(directory_path = temp_dir, SampleRate = SampleRate, SessionDuration = SessionDuration),
      "have a different number of rows than expected"
    )
    unlink(c("correct.txt", "short.txt"))
  })


test_that("Merge_PosData checks SessionDuration and raises errors if duration mismatches beyond tolerance", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  SampleRate <- 100
    SessionDuration <- 2
    # Expected rows = 200
    # Let's say 220 rows: actual_duration = 2.2s, difference = 0.2s > 0.1s tolerance
    data1 <- data.frame(
      Fx = rnorm(220),
      Fy = rnorm(220),
      Fz = rnorm(220, mean = -100),
      Mx = rnorm(220),
      My = rnorm(220),
      Mz = rnorm(220)
    )
    write.table(data1, file = "duration_mismatch.txt", row.names = FALSE, col.names = FALSE, sep = ",")

    expect_error(
      Merge_PosData(directory_path = temp_dir, SampleRate = SampleRate, SessionDuration = SessionDuration),
      regexp = "The following file\\(s\\) have a different number of rows than expected \\(SampleRate \\* SessionDuration\\):\\nduration_mismatch.txt"    )
    unlink("duration_mismatch.txt")
  })


test_that("Merge_PosData writes CSV if write_csv = TRUE", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  SampleRate <- 100
    # Just one file with correct rows
    data1 <- data.frame(
      Fx = rnorm(200),
      Fy = rnorm(200),
      Fz = rnorm(200, mean = -100),
      Mx = rnorm(200),
      My = rnorm(200),
      Mz = rnorm(200)
    )
    write.table(data1, file = "data.txt", row.names = FALSE, col.names = FALSE, sep = ",")

    result <- Merge_PosData(directory_path = temp_dir, SampleRate = SampleRate, write_csv = TRUE)
    expect_true(file.exists("OutPut_Postural_Data.csv"))
    csv_data <- read.csv("OutPut_Postural_Data.csv")
    # Check that the CSV data matches the output
    expect_equal(nrow(csv_data), nrow(result))
    expect_equal(names(csv_data), names(result))
    unlink("data.txt")

  })

