#' Compute Mean CoP-X and Mean CoP-Y for a specified time bin
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
#'#Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
#' path_to_data <- system.file("extdata", package = "BalanceMate")
#' Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331)
#'
#'# Compute mean CoP-X and mean CoP-Y at the participant (ID level)
#' Mean_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name")
#'
#' # Synthetise CoP-X and CoP-Y for 1s time bins
#' Mean_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name", time_col = "Time", epoch_length = 1)
#'
#' # Synthetise CoP-X and CoP-Y for 2s time bins
#' Mean_CoP_ComputeR(Data, "CoP_X", "CoP_Y", "file_name", time_col = "Time", epoch_length = 2)
#'
Mean_CoP_ComputeR <- function(data, CoPX_col, CoPY_col, ID, time_col = NULL, epoch_length = NULL) {
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
    if (((nrow(data)/length(table(participant_id)))/100/epoch_length) %% 1 != 0) {
      warning("Unbalanced epochs detected -- Epoch is not a multiple of protocol duration")
    }
  }

  # Split data by group
  data_list <- split(data, data$group)

  # Function to compute mean CoP-X and CoP-Y for each group
  compute_mean_cop <- function(subset_data) {
    if (nrow(subset_data) < 1) {
      return(c(mean_CoPX = NA, mean_CoPY = NA))
    } else {
      mean_copx <- mean(subset_data[[CoPX_col]], na.rm = TRUE)
      mean_copy <- mean(subset_data[[CoPY_col]], na.rm = TRUE)
      return(c(mean_CoPX = mean_copx, mean_CoPY = mean_copy))
    }
  }

  # Apply the function to each group
  mean_values <- do.call(rbind, lapply(data_list, compute_mean_cop))

  # Prepare the result based on whether epoch was used or not
  if (!is.null(epoch_length) && !is.null(time_col)) {
    # Split the group back into participant and epoch
    group_info <- do.call(rbind, strsplit(names(data_list), split = '\\.'))

    result <- data.frame(
      participant_id = group_info[, 1],
      epoch = as.numeric(group_info[, ncol(group_info)]),
      mean_CoPX = mean_values[, "mean_CoPX"],
      mean_CoPY = mean_values[, "mean_CoPY"],
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      participant_id = names(data_list),
      mean_CoPX = mean_values[, "mean_CoPX"],
      mean_CoPY = mean_values[, "mean_CoPY"],
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
