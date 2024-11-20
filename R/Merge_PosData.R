#' Merge and Append your Postural data
#'
#' @description
#' This function merges all .txt files directly exported from the AMTI NetForce software. It re-names columns (un-named by the AMTI software), compute automatically CoP on both the antero-posterior (CoP-Y) and medio-lateral (CoP-X) axes, and add the time course during the session to facilitate further data management. Finally, it saves the resulting output in your file directory.
#'
#' @param directory_path directory to a folder containing all the .txt outputs from AMTI Netforce that you wish to merge in a single data set.
#' @param SampleRate Sample rate used in your protocol
#' @param SessionDuration Duration of your protocol
#'
#' @return a dataframe containing all merged postural files, with additional columns: CoP-Y, CoP-X, time course, and file name.
#' @export
#'
#' @examples
#' #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
#' path_to_data <- system.file("extdata", package = "BalanceMate")
#'
#' # Input correct arguments: here, the protocol is 331seconds long, the sample rate is 100Hz
#' Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331)
Merge_PosData <- function(directory_path, SampleRate, SessionDuration) {
  # Step 1: Create a file list with all postural data files in the specified directory
  file_list <- list.files(path = directory_path, pattern = ".*\\.txt$", recursive = TRUE, full.names = TRUE)

  if(length(file_list) == 0) {
    stop("No .txt files found in the specified directory.")
  }
  if(is.na(SampleRate) || SampleRate <= 0) {
    stop("Invalid input for Sample Rate. Please enter a positive number.")
  }
  if(is.na(SessionDuration) || SessionDuration <= 0) {
    stop("Invalid input for Session Duration. Please enter a positive number.")
  }
  # Calculate expected number of rows based on the SampleRate and SessionDuration; this will allow warning generation in case users have weird data files
  expected_rows <- SampleRate * SessionDuration # Normally, each datafile should contain SampleRate*Session Duration rows.

  # Initialize a list to capture problematic files
  problematic_files <- list()

  # Read and stack the content of each file with a progress bar
  data_frames <- lapply(file_list, function(file) {
    # Read the file as a data frame, assuming no headers
    df <- read.table(file, header = FALSE, sep = ",", stringsAsFactors = FALSE)

    # Check if the number of rows in the file matches the expected number; quite smart
    if (nrow(df) != expected_rows) {
      # Capture the file name if the number of rows is incorrect
      problematic_files <<- c(problematic_files, basename(file))
    }

    # Add file name
    df$file_name <- basename(file) # Use just the file name, not the full path
    return(df)
  })

  # merge everything
  merged_data <- do.call(rbind, data_frames)

  # warning handling: weird data files in the mix.
  if (length(problematic_files) > 0) {
    warning("The following file(s) have a different number of rows than expected (SampleRate * SessionDuration):\n",
            paste(problematic_files, collapse = ", "))
  }

  # Step 3: Add column names and compute the Time, CoP, and SD CoP

  # 3.1. Naming the columns (assuming these are the variables in each .txt file)
  colnames(merged_data) <- c("Fx", "Fy", "Fz", "Mx", "My", "Mz", "file_name")

  # 3.2. Compute the Time column based on Sample Rate and Session Duration
  total_rows <- nrow(merged_data)
  merged_data$Time <- rep(seq(1/SampleRate, SessionDuration, length.out = SessionDuration * SampleRate),
                          length.out = total_rows)
  # 3.3. Compute CoP
  merged_data$CoP_X <- (-merged_data$My / merged_data$Fz)*100 # from meters to cm
  merged_data$CoP_Y <- (merged_data$Mx / merged_data$Fz)*100

  # Step 4: Write the resulting data frame to a CSV file in the same directory
  output_path <- file.path(directory_path, "OutPut_Postural_Data.csv")
  write.csv(merged_data, output_path, row.names = FALSE)

  # Return the merged data
  return(merged_data)
} # Results are consistent with custom code using dplyr et al.
