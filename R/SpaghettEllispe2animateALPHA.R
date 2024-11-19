# Load necessary package
if (!requireNamespace("animation", quietly = TRUE)) {
  install.packages("animation")
}
library(animation)

SpaghettEllipse2animate <- function(df, participant_id_col = "participant_id", participant_id = NULL,
                            time_col = "Time", time_start = NULL, time_end = NULL,
                            copx_col = "CoP_X", copy_col = "CoP_Y",
                            Title = "Spaghetti Plot with 95% Confidence Ellipse",
                            xlab = "Mean CoP_X", ylab = "Mean CoP_Y", conf_level = 0.95,
                            Animate = FALSE, animation_interval = 0.01, save_animation = FALSE, animation_file = "animation.gif") {
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

  # Check if there is enough data to compute covariance
  if (nrow(df) < 2) {
    stop("Not enough data to compute covariance and plot the ellipse.")
  }

  # Sort the data by time
  df <- df[order(df[[time_col]]), ]

  if (Animate) {
    # Set up the animation options
    ani.options(interval = animation_interval)

    # Create a temporary file to save the animation frames
    if (save_animation) {
      saveGIF({
        # Loop through each unique time point to create frames
        unique_times <- unique(df[[time_col]])
        for (t in unique_times) {
          # Subset data up to current time point
          df_subset <- df[df[[time_col]] <= t, ]

          # Aggregate the data to compute mean CoP_X and CoP_Y
          data2 <- aggregate(df_subset[, c(copx_col, copy_col)],
                             by = list(Time = df_subset[[time_col]]),
                             FUN = mean, na.rm = TRUE)

          # Rename the columns for clarity
          names(data2)[2:3] <- c("MeanCoPX", "MeanCoPY")

          # Create the plot
          plot(data2$MeanCoPX, data2$MeanCoPY,
               col = rgb(0, 0, 1, 0.5),
               pch = 16,
               cex = 0.7,
               xlab = xlab,
               ylab = ylab,
               main = paste(Title, "\nTime:", round(t, 2)),
               las = 1,
               bty = "l",
               cex.axis = 1.2,
               cex.lab = 1.5,
               cex.main = 1.5)

          # Add grid lines
          grid(col = "darkgrey", lty = "dotted")

          # Compute covariance and ellipse if enough data
          if (nrow(data2) >= 2) {
            cov_matrix <- cov(data2[, c("MeanCoPX", "MeanCoPY")])
            center <- colMeans(data2[, c("MeanCoPX", "MeanCoPY")])

            # Check if covariance matrix is valid
            if (!any(is.na(cov_matrix)) && det(cov_matrix) > 0) {
              eigen_decomp <- eigen(cov_matrix)
              eigenvalues <- eigen_decomp$values
              eigenvectors <- eigen_decomp$vectors

              chi_squared_value <- qchisq(conf_level, df = 2)
              axis_lengths <- sqrt(eigenvalues * chi_squared_value)

              theta <- seq(0, 2 * pi, length.out = 200)
              ellipse_coords <- t(center + (eigenvectors %*% diag(axis_lengths)) %*% rbind(cos(theta), sin(theta)))

              lines(ellipse_coords[, 1], ellipse_coords[, 2], col = "red", lwd = 2)
            }
          }
        }
      }, movie.name = animation_file)
      message("Animation saved as ", animation_file)
    } else {
      # Display animation without saving
      saveHTML({
        unique_times <- unique(df[[time_col]])
        for (t in unique_times) {
          # Subset data up to current time point
          df_subset <- df[df[[time_col]] <= t, ]

          # Aggregate the data to compute mean CoP_X and CoP_Y
          data2 <- aggregate(df_subset[, c(copx_col, copy_col)],
                             by = list(Time = df_subset[[time_col]]),
                             FUN = mean, na.rm = TRUE)

          # Rename the columns for clarity
          names(data2)[2:3] <- c("MeanCoPX", "MeanCoPY")

          # Create the plot
          plot(data2$MeanCoPX, data2$MeanCoPY,
               col = rgb(0, 0, 1, 0.5),
               pch = 16,
               cex = 0.7,
               xlab = xlab,
               ylab = ylab,
               main = paste(Title, "\nTime:", round(t, 2)),
               las = 1,
               bty = "l",
               cex.axis = 1.2,
               cex.lab = 1.5,
               cex.main = 1.5)

          # Add grid lines
          grid(col = "darkgrey", lty = "dotted")

          # Compute covariance and ellipse if enough data
          if (nrow(data2) >= 2) {
            cov_matrix <- cov(data2[, c("MeanCoPX", "MeanCoPY")])
            center <- colMeans(data2[, c("MeanCoPX", "MeanCoPY")])

            # Check if covariance matrix is valid
            if (!any(is.na(cov_matrix)) && det(cov_matrix) > 0) {
              eigen_decomp <- eigen(cov_matrix)
              eigenvalues <- eigen_decomp$values
              eigenvectors <- eigen_decomp$vectors

              chi_squared_value <- qchisq(conf_level, df = 2)
              axis_lengths <- sqrt(eigenvalues * chi_squared_value)

              theta <- seq(0, 2 * pi, length.out = 200)
              ellipse_coords <- t(center + (eigenvectors %*% diag(axis_lengths)) %*% rbind(cos(theta), sin(theta)))

              lines(ellipse_coords[, 1], ellipse_coords[, 2], col = "red", lwd = 2)
            }
          }
        }
      })
    }
  } else {
    # Non-animated plot
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

    # Create the plot
    plot(data2$MeanCoPX, data2$MeanCoPY,
         col = rgb(0, 0, 1, 0.5),
         pch = 16,
         cex = 0.7,
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

    # Optionally, display the computed ellipse area on the plot using the compute_ellipse_area function
    data2$participant_id <- 1  # Dummy participant ID for computation
    ellipse_result <- compute_ellipse_area(
      data = data2,
      CoPX_col = "MeanCoPX",
      CoPY_col = "MeanCoPY",
      participant_id_col = "participant_id",
      confint = conf_level
    )
    computed_area <- ellipse_result$ellipse_area

    legend("topright", legend = paste("Ellipse Area:", round(computed_area, 2)),
           bty = "n", cex = 1.2, text.col = "red")
  }
}
