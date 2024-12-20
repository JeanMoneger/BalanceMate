#' Compute mean Sway Path Length
#'
#' @param data a data frame
#' @param CoPX_col a column in the data frame storing the CoP-X data
#' @param CoPY_col a column in the data frame storing the CoP-Y data
#' @param ID a column in the data frame storing Participants' unique identifiers
#' @param time_col a column in the data frame storing Time course
#' @param epoch_length numeric value: epoch duration over which you want to compute the sway path length (in seconds)
#'
#' @details
#' If the specified epoch is not a multiple of the protocol duration (e.g., user inputs an epoch of 3s for a protocol of 10s), then the function will compute the SPL over the unbalanced time periods (in the latter example, 4 mean SPL would be computed with the three first periods corresponding to 3s intervals, but the last one corresponding to the remaining time -- that is 1s interval). A warning will be issued to inform user about unbalanced epoch computations.
#'
#'
#' @return a synthetised data frame (i.e., shorter) with an additional column (Sway path lenght)
#' @export
#'
#' @examples
#' # Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
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
#' lapply(files, function(f) {
#'   # Load the .Rdata file from the package's extdata directory
#'   load(file.path(data_dir, paste0(f, ".Rdata")))
#'
#'   data <- get(f)
#'   # Write the data to a .txt file in the temporary directory
#'   write.table(data, file = file.path(temp_data_dir, paste0(f, ".txt")), sep = ",",
#'   row.names = FALSE, col.names = FALSE, quote = FALSE)
#' })
#'
#' Data <- Merge_PosData(temp_data_dir, SampleRate = 100, SessionDuration = 100)
#'
#'SPL_ComputeR(data = Data,
#'     CoPX_col = "CoP_X",
#'     CoPY_col = "CoP_Y",
#'     ID = "file_name",
#'     time_col = "Time",
#'     epoch_length = 1)
#'
#'
SPL_ComputeR <- function(data, CoPX_col, CoPY_col, ID, time_col = NULL, epoch_length = NULL) {
  # Check if columns exist in the data frame
  if (!(CoPX_col %in% colnames(data))) stop(paste("Column", CoPX_col, "not found in the data frame"))
  if (!(CoPY_col %in% colnames(data))) stop(paste("Column", CoPY_col, "not found in the data frame"))
  if (!(ID %in% colnames(data))) stop(paste("Column", ID, "not found in the data frame"))

  # Extract necessary columns
  CoPX <- data[[CoPX_col]]
  CoPY <- data[[CoPY_col]]
  participant_id <- data[[ID]]

  # Create grouping variable
  if (!is.null(epoch_length) && !is.null(time_col)) {
    if (!(time_col %in% colnames(data))) stop(paste("Column", time_col, "not found in the data frame"))
    time <- data[[time_col]]
    # Create epochs
    max_epoch <- ceiling(max(time) / epoch_length)
    # Create epochs, ensuring they do not exceed max_epoch
    data$epoch <- pmin(floor(time / epoch_length) + 1, max_epoch)    # Create grouping variable for participant and epoch
    data$group <- interaction(participant_id, data$epoch, drop = TRUE)
  } else {
    # Group by participant only if no epoch is provided
    data$group <- as.character(participant_id)
  }
  # Incomplete epoch?
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

  # Function to compute sway path length for each group
  compute_sway <- function(subset_data) {
    if (nrow(subset_data) < 2) return(NA)  # If fewer than 2 data points, return NA
    dx <- diff(subset_data[[CoPX_col]])
    dy <- diff(subset_data[[CoPY_col]])
    distances <- sqrt(dx^2 + dy^2)
    sway_path_length <- sum(distances)
    return(sway_path_length)
  }

  # Apply the function to each group
  sway_values <- sapply(data_list, compute_sway)

  # Prepare the result based on whether epoch was used or not
  if (!is.null(epoch_length) && !is.null(time_col)) {
    # Split the group back into participant and epoch
    group_info <- do.call(rbind, strsplit(names(sway_values), split = '\\.'))

    result <- data.frame(
      participant_id = group_info[, 1],
      epoch = as.numeric(group_info[, ncol(group_info)]),
      sway_path_length = sway_values,
      stringsAsFactors = FALSE
    )
  } else {
    # No epoch, so only return participant and sway path length
    result <- data.frame(
      participant_id = names(sway_values),
      sway_path_length = sway_values,
      stringsAsFactors = FALSE
    )
  }

  # Remove rows where sway_path_length is NA (i.e., where there weren't enough data points)
  result <- result[!is.na(result$sway_path_length), ]

  # Order the result based on participant_id and epoch (if applicable)
  if ("epoch" %in% colnames(result)) {
    result <- result[order(result$participant_id, result$epoch), ]
  } else {
    result <- result[order(result$participant_id), ]
  }

  return(result)
}
