#' Apply Butterworth Filter to Specified Columns
#'
#' This function applies a Butterworth filter to specified columns in a data frame.
#'
#' @param Data A data frame containing the data to filter.
#' @param cutoff_freq The cutoff frequency for the filter.
#' @param filter_order The order of the Butterworth filter.
#' @param sampling_rate The sampling rate of the data.
#' @param type The type of the filter ("low" or "high"). Default is "low".
#' @param Colname A vector of column names to filter.
#'
#' @return A data frame with the filtered columns added, suffixed with "_filtered".
#' @export
#'
#' @importFrom signal butter filtfilt
#'
#' @examples
#' # Assuming 'Data' is your data frame with columns 'CoP_X' and 'CoP_Y'
#' filtered_data <- Butterworth_it(
#'   Data = Data,
#'   cutoff_freq = 5,
#'   filter_order = 2,
#'   sampling_rate = 100,
#'   type = "low",
#'   Colname = c("CoP_X", "CoP_Y")
#' )
#' # Visualise results:
#' plot(x = filtered_data$Time, y = filtered_data$CoP_Y, type ="l", tck = 1, main = "Non filtered")
#' plot(x = filtered_data$Time, y = filtered_data$CoP_Y_filtered, type ="l", tck = 1, main = "filtered")
Butterworth_it <- function(Data, cutoff_freq, filter_order, sampling_rate, type = "low", Colname) {
  # Validate inputs
  if (!is.data.frame(Data)) {
    stop("Data must be a data frame.")
  }
  if (!all(Colname %in% names(Data))) {
    missing_cols <- Colname[!Colname %in% names(Data)]
    stop(paste("The following columns are not in Data:", paste(missing_cols, collapse = ", ")))
  }
  if (!type %in% c("low", "high")) {
    stop("Type must be either 'low' or 'high'.")
  }

  # Create the Butterworth filter
  butter_filter <- signal::butter(
    n = filter_order,
    W = cutoff_freq / (sampling_rate / 2),
    type = type
  )

  # Apply the filter to the specified columns
  for (col in Colname) {
    # Apply zero-phase filtering
    filtered_signal <- signal::filtfilt(butter_filter, Data[[col]])
    # Add the filtered data to the Data frame
    Data[[paste0(col, "_filtered")]] <- filtered_signal
  }

  # Return the Data frame with filtered columns added
  return(Data)
}
