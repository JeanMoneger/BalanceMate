#' Compute Mean Standard deviations of CoP-X and CoP-Y displacements for a specified time bin
#'
#' @param data a dataframe containing the postural data
#' @param CoPX_col a column in the data frame storing the CoP-X data
#' @param CoPY_col a column in the data frame storing the CoP-Y data
#' @param ID a column in the data frame storing Participants' unique identifiers
#' @param time_col a column in the data frame storing Time course
#' @param epoch_length numeric value: epoch duration over which you want to compute the sway path length (in seconds)
#'
#' @return a data frame containing the new postural measures that were synthetised at the ID level or at the epoch level (if specified).
#' @export
#'
#' @examples
#' #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
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
#' # Compute mean SD CoP-X and mean SD CoP-Y at the participant (ID level):
#' SD_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name")
#'
#' # Synthetise SD CoP-X and SD CoP-Y for 1s time bins:
#' SD_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name", time_col = "Time", epoch_length = 1)
#'
#' # Synthetise SD CoP-X and SD CoP-Y for 2s time bins:
#' SD_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name", time_col = "Time", epoch_length = 2)
#'
SD_CoP_ComputeR <- function(data, CoPX_col, CoPY_col, ID, time_col = NULL, epoch_length = NULL) {
  # Check if columns exist in the data frame
  if (!(CoPX_col %in% colnames(data))) stop(paste("Column", CoPX_col, "not found in the data frame"))
  if (!(CoPY_col %in% colnames(data))) stop(paste("Column", CoPY_col, "not found in the data frame"))
  if (!(ID %in% colnames(data))) stop(paste("Column", ID, "not found in the data frame"))

  # Extract necessary columns
  participant_id <- data[[ID]]

  # Create grouping variable
  if (!is.null(epoch_length) && !is.null(time_col)) {
    if (!(time_col %in% colnames(data))) stop(paste("Column", time_col, "not found in the data frame"))
    time <- data[[time_col]]
    # Create epochs
    max_epoch <- ceiling(max(time) / epoch_length)
    data$epoch <- pmin(floor(time / epoch_length) + 1, max_epoch)
    # Create grouping variable for participant and epoch
    data$group <- interaction(participant_id, data$epoch, drop = TRUE)
  } else {
    # Group by participant only if no epoch is provided
    data$group <- as.character(participant_id)
  }

  # Incomplete epoch warning
  if (!is.null(epoch_length) && !is.null(time_col)) {
    time <- data[[time_col]]
    num_participants <- length(table(participant_id))
    rows_per_participant <- nrow(data) / num_participants

    # Compute sample rate
    sample_rate <- (rows_per_participant - 1) / (max(time) - min(time))

    # Check for unbalanced epochs
    if (((rows_per_participant) / sample_rate / epoch_length) %% 1 != 0) {
      warning("Unbalanced epochs detected -- Epoch is not a multiple of protocol duration")
    }
  }

  # Split data by group
  data_list <- split(data, data$group)

  # Function to compute SD of CoPX and CoPY for each group
  compute_sd <- function(subset_data) {
    if (nrow(subset_data) < 2) {
      return(c(SD_CoPX = NA, SD_CoPY = NA))  # Not enough data to compute SD
    } else {
      sd_copx <- sd(subset_data[[CoPX_col]], na.rm = TRUE)
      sd_copy <- sd(subset_data[[CoPY_col]], na.rm = TRUE)
      return(c(SD_CoPX = sd_copx, SD_CoPY = sd_copy))
    }
  }

  # Apply the function to each group and combine results
  sd_values <- do.call(rbind, lapply(data_list, compute_sd))

  # Prepare the result based on whether epoch was used or not
  if (!is.null(epoch_length) && !is.null(time_col)) {
    # Split the group back into participant and epoch
    group_info <- do.call(rbind, strsplit(names(data_list), split = '\\.'))

    result <- data.frame(
      participant_id = group_info[, 1],
      epoch = as.numeric(group_info[, ncol(group_info)]),
      SD_CoPX = sd_values[, "SD_CoPX"],
      SD_CoPY = sd_values[, "SD_CoPY"],
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      participant_id = names(data_list),
      SD_CoPX = sd_values[, "SD_CoPX"],
      SD_CoPY = sd_values[, "SD_CoPY"],
      stringsAsFactors = FALSE
    )
  }

  # Order the result based on participant_id and epoch (if applicable)
  if ("epoch" %in% colnames(result)) {
    result <- result[order(result$participant_id, result$epoch), ]
  } else {
    result <- result[order(result$participant_id), ]
  }

  return(result)
}
