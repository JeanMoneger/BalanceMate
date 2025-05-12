#' Compute X% ellipse area (cm.cm)
#'
#' @param data a data frame containing postural data (i.e., Center of pressure displacements on both the antero-posterior and medio-lateral axis)
#' @param CoPX_col a column from the data frame storing center of pressure displacements on the medio-lateral axis
#' @param CoPY_col a column from the data frame storing center of pressure displacements on the antero-posterior axis
#' @param ID a column from the data frame storing unique identifiers for each participant/session
#' @param time_col a column from the data frame storing time course over the session (optional; for computing results synthetised per epochs)
#' @param epoch_length an optional argument (if computing results synthetised at an epoch level) specifying length of the desired epoch
#' @param confint desired percentage for the confidence in the ellipse area (default is .95)
#'
#' @return a data frame with an additional 'ellipse_area' column
#' @export
#'
#' @importFrom grDevices rgb
#' @importFrom graphics grid legend lines
#' @importFrom stats aggregate cov qchisq sd
#' @importFrom utils read.table write.csv
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
#' compute_ellipse_area(Data, "CoP_X", "CoP_Y", ID = "file_name") # Individual level
#'
#' # Epochs of 1 second
#' compute_ellipse_area(Data, "CoP_X", "CoP_Y",
#'       ID = "file_name",
#'       time_col = "Time",
#'       epoch_length = 1)
#'
#'# Epochs of 1 second and 80% confidence Ellipse area
#' compute_ellipse_area(Data, "CoP_X", "CoP_Y",
#'       ID = "file_name",
#'       time_col = "Time",
#'       epoch_length = 1,
#'       confint = .80)

#'
compute_ellipse_area <- function(data, CoPX_col, CoPY_col, ID, time_col = NULL, epoch_length = NULL, confint = .95) {
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
    data$epoch <- pmin(floor(time / epoch_length) + 1, max_epoch)
    # Create grouping variable for participant and epoch
    data$group <- interaction(participant_id, data$epoch, drop = TRUE)  } else {
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




  # Split data by group (participant + epoch, or participant only)
  data_list <- split(data, data$group)

  # Chi-squared quantile for the specified confidence interval with 2 degrees of freedom
  chi_squared_quantile <- qchisq(confint, df = 2)

  # Function to compute ellipse area for each group
  compute_ellipse <- function(subset_data) {
    if (nrow(subset_data) < 2) {
      return(NA)  # Not enough data to compute covariance
    } else {
      cov_matrix <- cov(subset_data[, c(CoPX_col, CoPY_col)])
      eigenvalues <- eigen(cov_matrix)$values
      if (any(eigenvalues <= 0)) {
        return(NA)  # Covariance matrix is singular or not positive-definite
      } else {
        # Corrected formula for ellipse area
        ellipse_area <- pi * chi_squared_quantile * sqrt(prod(eigenvalues))
        return(ellipse_area)
      }
    }
  }
  # Apply the function to each group
  ellipse_values <- sapply(data_list, compute_ellipse)

  # Prepare the result based on whether epoch was used or not
  if (!is.null(epoch_length) && !is.null(time_col)) {
    # Split the group back into participant and epoch
    group_info <- do.call(rbind, strsplit(names(ellipse_values), split = '\\.'))

    # Ensure epochs are numeric
    result <- data.frame(
      participant_id = group_info[,1],
      epoch = as.numeric(group_info[, ncol(group_info)]),
      ellipse_area = ellipse_values,
      stringsAsFactors = FALSE
    )
  } else {
    # No epoch, so only return participant and ellipse area
    result <- data.frame(
      participant_id = names(ellipse_values),
      ellipse_area = ellipse_values,
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
} # Results are consistent with AMTI BioAnalysis outputs
