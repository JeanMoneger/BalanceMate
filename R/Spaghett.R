#' Create a "spaghetti plot" from your postural data.
#'
#' @description
#' This function creates a spaghetti plot - a plot that illustrates the CoP displacements stored in a data frame. The data frame should contain minimally a column storing the time course, a column storing CoP-X values, and a column storing CoP-Y values.
#'
#' @param df dataframe containing the data you want to plot
#' @param time_col column from dataframe that indicates the time course of postural measurements
#' @param copx_col column from dataframe that contains the CoP-X displacements
#' @param copy_col column from dataframe that contains the CoP-Y displacements
#' @param Title Character string specifying a title for your plot (optionnal)
#' @param xlab Character string specifying the x-axis name (optional)
#' @param ylab Character string specifying the y-axis name (optional)
#'
#' @return a graphical object
#' @export
#'
#' @examples
#' data(Postural_DataA) # Load a session to plot
#' Spaghett(Postural_DataA, time_col = "Time", copx_col = "CoP_X", copy_col = "CoP_Y")
#'
#'
Spaghett <- function(df, time_col = "Time", copx_col = "CoP_X", copy_col = "CoP_Y", Title = "Spaghetti plot", xlab = "Mean CoP_X", ylab = "Mean CoP_Y") {
  # Ensure the specified columns exist in the dataframe
  if (!all(c(time_col, copx_col, copy_col) %in% names(df))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }

  # Aggregate the data to compute mean CoP_X and CoP_Y for each time point
  data2 <- aggregate(df[, c(copx_col, copy_col)],
                     by = list(Time = df[[time_col]]),
                     FUN = mean, na.rm = TRUE)

  # Rename the columns for clarity
  names(data2)[2:3] <- c("MeanCoPX", "MeanCoPY")

  # Create the plot using base R plotting functions
  plot(data2$MeanCoPX, data2$MeanCoPY,
       col = rgb(0, 0, 1, 0.1),
       pch = 16,
       cex = 0.5,
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
}
