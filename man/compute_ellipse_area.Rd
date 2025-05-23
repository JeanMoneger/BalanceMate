% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EllipseArea.R
\name{compute_ellipse_area}
\alias{compute_ellipse_area}
\title{Compute X\% ellipse area (cm.cm)}
\usage{
compute_ellipse_area(
  data,
  CoPX_col,
  CoPY_col,
  ID,
  time_col = NULL,
  epoch_length = NULL,
  confint = 0.95
)
}
\arguments{
\item{data}{a data frame containing postural data (i.e., Center of pressure displacements on both the antero-posterior and medio-lateral axis)}

\item{CoPX_col}{a column from the data frame storing center of pressure displacements on the medio-lateral axis}

\item{CoPY_col}{a column from the data frame storing center of pressure displacements on the antero-posterior axis}

\item{ID}{a column from the data frame storing unique identifiers for each participant/session}

\item{time_col}{a column from the data frame storing time course over the session (optional; for computing results synthetised per epochs)}

\item{epoch_length}{an optional argument (if computing results synthetised at an epoch level) specifying length of the desired epoch}

\item{confint}{desired percentage for the confidence in the ellipse area (default is .95)}
}
\value{
a data frame with an additional 'ellipse_area' column
}
\description{
Compute X\% ellipse area (cm.cm)
}
\examples{
#Find subdirectory of Example data in the original .txt format exported from AMTI Netforce software
# Note: we need to convert compressed RData to original txt file
files <- paste0("Postural_Data", LETTERS[1:6])

# Locate the directory containing the .RData files within the package
data_dir <- system.file("extdata", package = "BalanceMate")

# Create a temporary directory to store the .txt files
temp_data_dir <- file.path(tempdir(), "extdata")
dir.create(temp_data_dir, showWarnings = FALSE)

# Process each file: load, optionally add a blank row, and write to .txt
invisible(lapply(files, function(f) {
  # Load the .RData file from the package's extdata directory
  load(file.path(data_dir, paste0(f, ".RData")))

  data <- get(f)
  # Write the data to a .txt file in the temporary directory
  write.table(data, file = file.path(temp_data_dir, paste0(f, ".txt")), sep = ",",
  row.names = FALSE, col.names = FALSE, quote = FALSE)
}))

Data <- Merge_PosData(temp_data_dir, SampleRate = 100, SessionDuration = 100)

compute_ellipse_area(Data, "CoP_X", "CoP_Y", ID = "file_name") # Individual level

# Epochs of 1 second
compute_ellipse_area(Data, "CoP_X", "CoP_Y",
      ID = "file_name",
      time_col = "Time",
      epoch_length = 1)

# Epochs of 1 second and 80\% confidence Ellipse area
compute_ellipse_area(Data, "CoP_X", "CoP_Y",
      ID = "file_name",
      time_col = "Time",
      epoch_length = 1,
      confint = .80)

}
