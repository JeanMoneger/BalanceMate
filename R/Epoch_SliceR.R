#' Synthetise data as a function of defined Epoch
#'
#' @description
#' This function will synthetise (i.e., compute the mean of) defined column(s) from a data frame depending on a specified epoch (i.e., a time interval over which we want to synthetise the data).
#'
#'
#' @param df a data frame containing the data we want to synthetise at a defined epoch-level
#' @param ID a column from the data frame referencing the unique participants' identifiers
#' @param columns_to_synthesize columns of the data frame that we wish to synthetise at the epoch-level
#' @param epoch_length time duration over which we want to synthetise the data (in seconds)
#' @param sample_rate sample rate of the protocol
#' @param session_duration duration of the whole protocol
#'
#' @return a data frame that should be shorter.
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
#' Epoch_SliceR(df = Data,
#'       ID = "file_name",
#'       columns_to_synthesize = c("CoP_X", "CoP_Y"),
#'       epoch_length = 1,
#'       sample_rate=100,
#'       session_duration = 100)
#'
#'

Epoch_SliceR <- function(df, ID, columns_to_synthesize, epoch_length, sample_rate, session_duration) {
  # Calculate samples per epoch and expected samples per participant
  samples_per_epoch <- sample_rate * epoch_length
  expected_samples_per_participant <- sample_rate * session_duration

  # Get unique participant IDs
  participant_ids <- unique(df[[ID]])

  # Initialize list to hold results
  result_list <- list()

  # Process each participant individually
  for (participant in participant_ids) {
    # Subset data for the participant
    participant_data <- df[df[[ID]] == participant, ]
    actual_samples <- nrow(participant_data)

    # Check if actual samples match expected samples
    if (actual_samples != expected_samples_per_participant) {
      warning(sprintf("Participant '%s' has %d samples; expected %d samples.",
                      participant, actual_samples, expected_samples_per_participant))
    }

    # Assign epoch numbers using integer division
    epoch_numbers <- ((seq_len(actual_samples) - 1) %/% samples_per_epoch) + 1
    participant_data$Epoch <- epoch_numbers

    # Warn if the last epoch is incomplete
    last_epoch_samples <- actual_samples %% samples_per_epoch
    if (last_epoch_samples > 0 && last_epoch_samples < samples_per_epoch) {
      warning(sprintf("Participant '%s' has an incomplete final epoch with only %d samples (expected %d samples per epoch).",
                      participant, last_epoch_samples, samples_per_epoch))
    }

    # Initialize result data frame for this participant
    unique_epochs <- unique(participant_data$Epoch)
    participant_result <- data.frame(ID = participant, Epoch = unique_epochs)

    # For columns to synthesize, compute mean and sd per epoch
    for (col in columns_to_synthesize) {
      # Compute mean per epoch
      mean_result <- aggregate(participant_data[[col]], by = list(Epoch = participant_data$Epoch),
                               FUN = mean, na.rm = TRUE)
      colnames(mean_result)[2] <- paste0("Mean_", col)

      # Compute sd per epoch
      sd_result <- aggregate(participant_data[[col]], by = list(Epoch = participant_data$Epoch),
                             FUN = sd, na.rm = TRUE)
      colnames(sd_result)[2] <- paste0("SD_", col)

      # Merge mean and sd into participant_result
      participant_result <- merge(participant_result, mean_result, by = "Epoch", all.x = TRUE)
      participant_result <- merge(participant_result, sd_result, by = "Epoch", all.x = TRUE)
    }

    # For other columns, take the first value within each epoch
    other_columns <- setdiff(names(participant_data), c(ID, "Epoch", columns_to_synthesize))
    if (length(other_columns) > 0) {
      for (col in other_columns) {
        first_values <- aggregate(participant_data[[col]], by = list(Epoch = participant_data$Epoch),
                                  FUN = function(x) x[1])
        colnames(first_values)[2] <- col

        # Merge into participant_result
        participant_result <- merge(participant_result, first_values, by = "Epoch", all.x = TRUE)
      }
    }

    # Add to result list
    result_list[[as.character(participant)]] <- participant_result
  }

  # Combine all participant results into one data frame
  result <- do.call(rbind, result_list)

  # Reset row names
  rownames(result) <- NULL

  # Return the result
  return(result)
}
