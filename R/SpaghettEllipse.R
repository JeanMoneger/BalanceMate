#' Make a Spaghetti plot and draw an X% confidence ellipse area
#'
#' @description
#' A function to output a so-called 'spaghetti plot' indicating CoP-X and CoP-Y movements over a specific time period, with a X% confidence ellipse area (default = 95%) over the spaghetti.
#' A 95% confidence ellipse will circle 95% of the data points observed.
#'
#' @param df a data frame containing the data we want to output
#' @param participant_id_col a column from the data frame containing participants' unique identifiers
#' @param participant_id character string: a specific participant that we would like to plot (optionnal, if not specified, the function will plot all participants)
#' @param time_col a column from the data frame storing time course during the session
#' @param time_start numeric value: indicate the start of the period you want to plot (optionnal, in unspecified, the function will plot the whole duration of the session)
#' @param time_end numeric value: indicate the end of the period you want to plot (optionnal, in unspecified, the function will plot the whole duration of the session)
#' @param copx_col a column from the data frame storing the CoP-X data
#' @param copy_col a column from the data frame storing the CoP-Y data
#' @param Title an optionnal character string to customize title of the plot
#' @param xlab an optionnal character string to customize x-axis of the plot
#' @param ylab an optionnal character string to customize y-axis of the plot
#' @param conf_level numeric value: Percentage of confidence of the ellipse area (default = .95)
#'
#' @return a graphical object
#' @export
#'
#' @examples
#' path_to_data <- system.file("extdata", package = "BalanceMate") #Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
#' Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331) # Input correct arguments: in this example, the protocol was 331seconds long and the sample rate was 100Hz
#'
#' SpaghettEllipse(Data, participant_id_col = "file_name", participant_id = "Postural_DataB.txt")
#' # Currently, it "works", except it outputs an error... but still does the job?
SpaghettEllipse <- function(df, participant_id_col = "participant_id", participant_id = NULL,
                            time_col = "Time", time_start = NULL, time_end = NULL,
                            copx_col = "CoP_X", copy_col = "CoP_Y",
                            Title = "Spaghetti Plot with 95% Confidence Ellipse",
                            xlab = "Mean CoP_X", ylab = "Mean CoP_Y", conf_level = 0.95) {
  # Ensure the specified columns exist in the dataframe
  required_cols <- c(time_col, copx_col, copy_col, participant_id_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("The following specified columns do not exist in the dataframe:", paste(missing_cols, collapse = ", ")))
  }

  # Filter the dataframe based on participant_id if provided
  if (!is.null(participant_id)) {
    df <- df[df[[participant_id_col]] == participant_id, ]
    if (nrow(df) == 0) {
      stop("No data found for the specified participant ID.")
    }
  }

  # Filter the dataframe based on time range if provided
  if (!is.null(time_start) && !is.null(time_end)) {
    df <- df[df[[time_col]] >= time_start & df[[time_col]] <= time_end, ]
    if (nrow(df) == 0) {
      stop("No data found in the specified time range.")
    }
  }

  # Aggregate the data to compute mean CoP_X and CoP_Y for each time point
  data2 <- aggregate(df[, c(copx_col, copy_col)],
                     by = list(Time = df[[time_col]]),
                     FUN = mean, na.rm = TRUE)

  # Check if there is enough data to compute covariance
  if (nrow(data2) < 2) {
    stop("Not enough data to compute covariance and plot the ellipse.")
  }

  # Rename the columns for clarity
  names(data2)[2:3] <- c("MeanCoPX", "MeanCoPY")

  # Create the plot using base R plotting functions
  plot(data2$MeanCoPX, data2$MeanCoPY,
       col = scales::alpha(rgb(0, 0, 1, 0.5), .3),
       type = "l",
       #pch = 4,
       #cex = 0.6,
       xlab = xlab,
       ylab = ylab,
       main = Title,
       las = 1,
       bty = "l",
       cex.axis = 1.2,
       cex.lab = 1.5,
       cex.main = 1.5)

  # Add grid lines for better readability
  grid(col = "darkgrey", lty = "dotted")

  # Compute the covariance matrix and mean of MeanCoPX and MeanCoPY
  cov_matrix <- cov(data2[, c("MeanCoPX", "MeanCoPY")])
  center <- colMeans(data2[, c("MeanCoPX", "MeanCoPY")])

  # Check if covariance matrix is valid
  if (any(is.na(cov_matrix)) || det(cov_matrix) <= 0) {
    warning("Covariance matrix is singular or not positive-definite. Cannot compute ellipse.")
    return()
  }

  # Eigen decomposition of the covariance matrix
  eigen_decomp <- eigen(cov_matrix)
  eigenvalues <- eigen_decomp$values
  eigenvectors <- eigen_decomp$vectors

  # Chi-squared value for the specified confidence level with 2 degrees of freedom
  chi_squared_value <- qchisq(conf_level, df = 2)

  # Calculate the lengths of the semi-axes
  axis_lengths <- sqrt(eigenvalues * chi_squared_value)

  # Generate points on the ellipse
  theta <- seq(0, 2 * pi, length.out = 200)
  ellipse_coords <- t(center + (eigenvectors %*% diag(axis_lengths)) %*% rbind(cos(theta), sin(theta)))

  # Add the 95% confidence ellipse to the plot
  lines(ellipse_coords[, 1], ellipse_coords[, 2], col = "red", lwd = 2)
} #Plot is identical to the one produced by BioAnalysis
