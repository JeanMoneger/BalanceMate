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

test <- Merge_PosData(directory_path = "ExampleData", SampleRate = 100, SessionDuration = 331 )


Spaghett(test)
