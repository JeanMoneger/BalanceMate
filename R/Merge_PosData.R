#' Merge and Append your Postural data
#'
#' @description
#' This function merges all .txt files directly exported from the AMTI NetForce software. It re-names columns (un-named by the AMTI software), compute automatically CoP on both the antero-posterior (CoP-Y) and medio-lateral (CoP-X) axes, and add the time course during the session to facilitate further data management. Finally, it optionnally saves the resulting output in your file directory. Note that the SessionDuaration argument is optionnal and only serves the purpose of verifying data integrity: if you have sessions of different length, you should not use it. However if your sessions are supposed to be of equal length, then it might make sense to use the argument in order to assess whether the number of rows expected from Sample rate and Session duration match the Actual duration of the sessions.
#'
#' @param directory_path directory to a folder containing all the .txt outputs from AMTI Netforce that you wish to merge in a single data set.
#' @param SampleRate Sample rate used in your protocol
#' @param SessionDuration Duration of your protocol (optionnal)
#' @param write_csv Logical. If TRUE, the merged data is saved as a CSV file in the same directory. Default is FALSE.
#'
#' @return a dataframe containing all merged postural files, with additional columns: CoP-Y, CoP-X, time course, and file name.
#' @export
#'
#' @examples
#' #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
#' # Note: we need to convert compressed rdata to original txt file
#' files <- paste0("Postural_Data", LETTERS[1:6])
#'
#' # Locate the directory containing the .Rdata files within the package
#' data_dir <- system.file("data", package = "BalanceMate")
#'
#' # Create a temporary directory to store the .txt files
#' temp_data_dir <- file.path(tempdir(), "data")
#' dir.create(temp_data_dir, showWarnings = FALSE)
#'
#' # Process each file: load, optionally add a blank row, and write to .txt
#' invisible(lapply(files, function(f) {
#'   # Load the .Rdata file from the package's extdata directory
#'   load(file.path(data_dir, paste0(f, ".Rdata")))
#'
#'   data <- get(f)
#'   # Write the data to a .txt file in the temporary directory
#'   write.table(data, file = file.path(temp_data_dir, paste0(f, ".txt")), sep = ",",
#'   row.names = FALSE, col.names = FALSE, quote = FALSE)
#' }))
#'
#' Data <- Merge_PosData(temp_data_dir, SampleRate = 100, SessionDuration = 100)
#'
#'
Merge_PosData <- function(directory_path, SampleRate, SessionDuration = NULL, write_csv = FALSE) {
  # Step 1: Create a file list with all postural data files in the specified directory
  file_list <- list.files(path = directory_path, pattern = ".*\\.txt$", recursive = TRUE, full.names = TRUE)

  if (length(file_list) == 0) {
    stop("No .txt files found in the specified directory.")
  }
  if (is.na(SampleRate) || SampleRate <= 0) {
    stop("Invalid input for SampleRate. Please enter a positive number.")
  }
  if (!is.null(SessionDuration) && (is.na(SessionDuration) || SessionDuration <= 0)) {
    stop("Invalid input for SessionDuration. Please enter a positive number.")
  }

  # Initialize lists to capture problematic files
  problematic_files <- list()
  duration_problem_files <- list()
  tolerance <- 0.1  # Tolerance in seconds

  # Read and stack the content of each file
  data_frames <- lapply(file_list, function(file) {
    # Read the file as a data frame, assuming no headers
    df <- read.table(file, header = FALSE, sep = ",", stringsAsFactors = FALSE)

    # Compute actual duration
    actual_duration <- nrow(df) / SampleRate

    # Perform checks only if SessionDuration is provided
    if (!is.null(SessionDuration)) {
      # Check if the number of rows in the file matches the expected number
      expected_rows <- SampleRate * SessionDuration
      if (nrow(df) != expected_rows) {
        # Capture the file name if the number of rows is incorrect
        problematic_files <<- c(problematic_files, basename(file))
      }

      # Check if the actual duration matches the SessionDuration
      if (abs(actual_duration - SessionDuration) > tolerance) {
        duration_problem_files <<- c(duration_problem_files, basename(file))
      }
    }

    # Add Time column
    df$Time <- seq(1 / SampleRate, actual_duration, length.out = nrow(df))

    # Add file name
    df$file_name <- basename(file)  # Use just the file name, not the full path
    return(df)
  })

  # Check for errors before proceeding (only if SessionDuration is provided)
  if (!is.null(SessionDuration)) {
    if (length(problematic_files) > 0) {
      stop(
        "The following file(s) have a different number of rows than expected (SampleRate * SessionDuration):\n",
        paste(problematic_files, collapse = ", ")
      )
    }

    if (length(duration_problem_files) > 0) {
      stop(
        "The following file(s) have a different duration than expected (SessionDuration):\n",
        paste(duration_problem_files, collapse = ", ")
      )
    }
  }

  # Merge all data frames
  merged_data <- do.call(rbind, data_frames)

  # Step 3: Add column names and compute CoP
  # 3.1. Naming the columns
  colnames(merged_data) <- c("Fx", "Fy", "Fz", "Mx", "My", "Mz", "Time", "file_name")

  # 3.2. Compute CoP
  merged_data$CoP_X <- (-merged_data$My / merged_data$Fz) * 100  # from meters to cm
  merged_data$CoP_Y <- (merged_data$Mx / merged_data$Fz) * 100

  # Step 4: Optionally write the resulting data frame to a CSV file in the same directory
  if (write_csv) {
    output_path <- file.path(directory_path, "OutPut_Postural_Data.csv")
    write.csv(merged_data, output_path, row.names = FALSE)
  }

  # Return the merged data
  return(merged_data)
}
