#' CoPY_ComputeR
#' @description
#' A function to compute Center of pressure (CoP) values on the antero-posterioir axis (Y-axis), a.k.a. CoP-Y. The function only requires moments of forces on the X-axis and Force on the Z-axis which are provided in most postural tools outputs.
#'
#' @param Mx numeric vector specifying the moment on the Y-axis
#' @param Fz numeric vector specifying the force on the Z-axis
#'
#' @return A numeric vector containing the Center of pressure values (cm) on the Y-axis
#' @export
#'
#' @seealso [CoPX_ComputeR()] for computing CoP-X values
#'
#' @examples
#' # Note: we need to convert compressed RData to original txt file
#' files <- paste0("Postural_Data", LETTERS[1:6])
#'
#' # Locate the directory containing the .RData files within the package
#' data_dir <- system.file("extdata", package = "BalanceMate")
#'
#' # Create a temporary directory to store the .txt files
#' temp_data_dir <- file.path(tempdir(), "extdata")
#' dir.create(temp_data_dir, showWarnings = FALSE)
#'
#' # Process each file: load, optionally add a blank row, and write to .txt
#' invisible(lapply(files, function(f) {
#'   # Load the .RData file from the package's extdata directory
#'   load(file.path(data_dir, paste0(f, ".RData")))
#'
#'   data <- get(f)
#'   # Write the data to a .txt file in the temporary directory
#'   write.table(data, file = file.path(temp_data_dir, paste0(f, ".txt")), sep = ",",
#'   row.names = FALSE, col.names = FALSE, quote = FALSE)
#' }))
#'
#' Data <- Merge_PosData(temp_data_dir, SampleRate = 100, SessionDuration = 100)
#'
#' CoPY_ComputeR(Data$Mx, Data$Fz)
CoPY_ComputeR <- function(Mx, Fz) {

  # Error if length mismatch
  if (length(Mx) != length(Fz)) {
    stop("Vectors 'Mx' and 'Fz' must be of the same length.")
  }

  # Error handling for division by zero
  if (any(Fz == 0)) {
    stop("Fz cannot be zero to compute CoP Y.")
  }

  copY <- (Mx / Fz)*100
  return(copY)
}

