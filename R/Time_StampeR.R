#' Annotate your Postural file to describe important periods
#'
#' @description
#' This function allows user to annotate (i.e., add a column to the data frame) a dataframe to label important periods in the session. For instance, if your protocol included training (10s) and trial (30s), you can use the `Time_StampeR` to add a column that will annotate the training measures and the trial measures.
#'
#' @param df a data frame containing your postural data
#' @param id_col a column from the data frame that store the participants' unique identifiers
#' @param sample_rate the sample rate used in your protocol
#' @param protocol_duration the duration of your protocols
#' @param cuts a list of numeric values that indicate where your protocol should be split to distinguish between two different periods.
#' @param period_names a list of character strings that will be used as labels for your split. There should be one more label than cuts.
#'
#' @return a data frame with an additional column labelling each time measurement.
#' @export
#'
#' @examples
#' # Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
#' # Note: we need to convert compressed RData to original txt file
#' files <- paste0("Postural_Data", LETTERS[1:6])
#'
#' # Locate the directory containing the .RData files within the package
#' data_dir <- system.file("extdata", package = "BalanceMate")
#'
#' # Create a temporary directory to store the .txt files
#' temp_data_dir <- file.path(tempdir(), "extdata")
#' dir.create(temp_data_dir, showWarnings = FALSE)
#'
#' # Process each file: load, optionally add a blank row, and write to .txt
#' invisible(lapply(files, function(f) {
#'   # Load the .RData file from the package's extdata directory
#'   load(file.path(data_dir, paste0(f, ".RData")))
#'
#'   data <- get(f)
#'   # Write the data to a .txt file in the temporary directory
#'   write.table(data, file = file.path(temp_data_dir, paste0(f, ".txt")), sep = ",",
#'   row.names = FALSE, col.names = FALSE, quote = FALSE)
#' }))
#'
#' Data <- Merge_PosData(temp_data_dir, SampleRate = 100, SessionDuration = 100)
#'
#' # here session: 30 s training, 3 trials (10 s fix cross + 90 s trial), 1s blank
#' cuts = c(30, 40, 130, 140, 230, 240, 330)
#' labels = c("Training",
#'       "FixationCross",
#'       "trial1",
#'       "FixationCross",
#'       "trial2",
#'       "FixationCross",
#'       "trial3",
#'       "blank")
#' Data <- Time_StampeR(df = Data,
#'       id_col = "file_name",
#'       sample_rate = 100,
#'       protocol_duration = 100,
#'       cuts = cuts,
#'       period_names = labels)
#'
#'
Time_StampeR <- function(df, id_col, sample_rate, protocol_duration, cuts = NULL, period_names = NULL) {
  # Get unique participant IDs
  participant_ids <- unique(df[[id_col]])

  # Expected number of measurements per participant
  expected_n_measurements <- sample_rate * protocol_duration

  # Initialize Time column
  df$Time <- NA

  # Initialize Period_Name column if periods are provided
  if (!is.null(cuts) && !is.null(period_names)) {
    df$Period_Name <- NA
  }

  # Process each participant
  for (participant in participant_ids) {
    participant_indices <- which(df[[id_col]] == participant)
    n_rows <- length(participant_indices)

    # Generate time vector for this participant
    time_vector <- seq(from = 0, by = 1 / sample_rate, length.out = n_rows)
    df$Time[participant_indices] <- time_vector

    # Check if the number of measurements matches expected
    if (n_rows != expected_n_measurements) {
      warning(sprintf("Participant %s has %d rows, expected %d. Time stamped it anyway.", participant, n_rows, expected_n_measurements))
    }

    # Assign periods if definitions are provided
    if (!is.null(cuts) && !is.null(period_names)) {
      # Create a vector of cut points including start and end times
      cut_points <- c(0, cuts, protocol_duration)

      # Check if the number of period names matches the number of intervals
      if (length(period_names) != length(cut_points) - 1) {
        stop("The number of period names must be one more than the number of cut points (including 0 and protocol_duration).")
      }

      # Assign periods
      for (i in seq_along(period_names)) {
        start_time <- cut_points[i]
        end_time <- cut_points[i + 1]
        indices <- participant_indices[df$Time[participant_indices] >= start_time & df$Time[participant_indices] < end_time]
        df$Period_Name[indices] <- period_names[i]
      }
    }
  }

  # Warn if any Period_Name is still NA
  if (!is.null(cuts) && !is.null(period_names) && any(is.na(df$Period_Name))) {
    warning("Some time points were not assigned to any period.")
  }

  return(df)
} # Work.
