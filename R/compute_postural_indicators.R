#' Compute Postural indicators for specified time periods
#'
#' @param data a dataframe containing the postural data
#' @param CoPX_col a column in the data frame storing the CoP-X data
#' @param CoPY_col a column in the data frame storing the CoP-Y data
#' @param ID a column in the data frame storing Participants' unique identifiers
#' @param time_col a column in the data frame storing Time course
#' @param epoch_length numeric value: epoch duration over which you want to compute the sway path length (in seconds)
#' @param indicators character string specifying the postural indicator to compute (can take multiple values). Possible values are "CoP_X", "CoP_Y", "SwayPathLength", "EllipseArea", "SD_CoP_X", and/or "SD_CoP_Y".
#'
#' @return a data frame containing the new postural measures that was synthetised at the ID level or at the epoch level (if specified).
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
#' # Using the Epoch synthesis
#' result <- compute_postural_indicators(data = Data,
#'      CoPX_col = "CoP_X",
#'      CoPY_col = "CoP_Y",
#'      ID = "file_name",
#'      time_col = "Time",
#'      epoch_length = 1,
#'      indicators = c("CoP_X", "CoP_Y", "SwayPathLength", "EllipseArea", "SD_CoP_X", "SD_CoP_Y"))
#' print(result)
#'
#' # Not using epochs: synthetising at the ID level
#' result <- compute_postural_indicators(data = Data,
#'       CoPX_col = "CoP_X",
#'       CoPY_col = "CoP_Y",
#'       ID = "file_name",
#'       indicators = c("CoP_X", "CoP_Y", "SwayPathLength", "EllipseArea", "SD_CoP_X", "SD_CoP_Y"))
#' print(result)
compute_postural_indicators <- function(data, CoPX_col, CoPY_col, ID, time_col = NULL, epoch_length = NULL, indicators) {
  allowed_indicators <- c("CoP_X", "CoP_Y", "SwayPathLength", "EllipseArea", "SD_CoP_X", "SD_CoP_Y")

  # Validate indicators
  if (!all(indicators %in% allowed_indicators)) {
    invalid_inds <- indicators[!indicators %in% allowed_indicators]
    stop(paste("Invalid indicator(s):", paste(invalid_inds, collapse = ", "),
               "\nValid options are:", paste(allowed_indicators, collapse = ", ")))
  }

  # Initialize result data frame
  result <- NULL

  # Helper function to merge results
  merge_results <- function(res1, res2) {
    if (is.null(res1)) {
      return(res2)
    } else {
      # Merge on participant_id and epoch (if applicable)
      by_cols <- "participant_id"
      if ("epoch" %in% colnames(res1) && "epoch" %in% colnames(res2)) {
        by_cols <- c(by_cols, "epoch")
      }
      res_merged <- merge(res1, res2, by = by_cols, all = TRUE)
      return(res_merged)
    }
  }

  # Loop through each indicator and compute
  for (indicator in indicators) {
    if (indicator == "EllipseArea") {
      temp_result <- compute_ellipse_area(data, CoPX_col, CoPY_col, ID, time_col, epoch_length)
      # Rename column
      colnames(temp_result)[colnames(temp_result) == "ellipse_area"] <- "EllipseArea"

      # Select relevant columns
      cols_to_select <- c("participant_id")
      if ("epoch" %in% colnames(temp_result)) {
        cols_to_select <- c(cols_to_select, "epoch")
      }
      cols_to_select <- c(cols_to_select, "EllipseArea")
      temp_result <- temp_result[, cols_to_select, drop = FALSE]

    } else if (indicator == "SwayPathLength") {
      temp_result <- SPL_ComputeR(data, CoPX_col, CoPY_col, ID, time_col, epoch_length)
      # Rename column
      colnames(temp_result)[colnames(temp_result) == "sway_path_length"] <- "SwayPathLength"

      # Select relevant columns
      cols_to_select <- c("participant_id")
      if ("epoch" %in% colnames(temp_result)) {
        cols_to_select <- c(cols_to_select, "epoch")
      }
      cols_to_select <- c(cols_to_select, "SwayPathLength")
      temp_result <- temp_result[, cols_to_select, drop = FALSE]

    } else if (indicator %in% c("SD_CoP_X", "SD_CoP_Y")) {
      temp_result <- SD_CoP_ComputeR(data, CoPX_col, CoPY_col, ID, time_col, epoch_length)
      # Rename columns if necessary
      cols_to_select <- c("participant_id")
      if ("epoch" %in% colnames(temp_result)) {
        cols_to_select <- c(cols_to_select, "epoch")
      }
      if (indicator == "SD_CoP_X") {
        cols_to_select <- c(cols_to_select, "SD_CoPX")
        temp_result <- temp_result[, cols_to_select, drop = FALSE]
        colnames(temp_result)[colnames(temp_result) == "SD_CoPX"] <- "SD_CoP_X"
      } else {
        cols_to_select <- c(cols_to_select, "SD_CoPY")
        temp_result <- temp_result[, cols_to_select, drop = FALSE]
        colnames(temp_result)[colnames(temp_result) == "SD_CoPY"] <- "SD_CoP_Y"
      }

    } else if (indicator %in% c("CoP_X", "CoP_Y")) {
      temp_result <- Mean_CoP_ComputeR(data, CoPX_col, CoPY_col, ID, time_col, epoch_length)
      # Rename columns if necessary
      cols_to_select <- c("participant_id")
      if ("epoch" %in% colnames(temp_result)) {
        cols_to_select <- c(cols_to_select, "epoch")
      }
      if (indicator == "CoP_X") {
        cols_to_select <- c(cols_to_select, "mean_CoPX")
        temp_result <- temp_result[, cols_to_select, drop = FALSE]
        colnames(temp_result)[colnames(temp_result) == "mean_CoPX"] <- "CoP_X"
      } else {
        cols_to_select <- c(cols_to_select, "mean_CoPY")
        temp_result <- temp_result[, cols_to_select, drop = FALSE]
        colnames(temp_result)[colnames(temp_result) == "mean_CoPY"] <- "CoP_Y"
      }
    }
    # Merge the current result with the accumulated result
    result <- merge_results(result, temp_result)
  }

  # Order the result based on participant_id and epoch (if applicable)
  if ("epoch" %in% colnames(result)) {
    result <- result[order(result$participant_id, result$epoch), ]
  } else {
    result <- result[order(result$participant_id), ]
  }

  return(result)
}
