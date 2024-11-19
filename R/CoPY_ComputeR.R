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
#' @examples path_to_data <- system.file("extdata", package = "BalanceMate")
#' Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 331)
#'
#' CoPY_ComputeR(Data$Mx, Data$Fz)
CoPY_ComputeR <- function(Mx, Fz) {
  # Error handling for division by zero
  if (any(Fz == 0)) {
    stop("Fz cannot be zero to compute CoP Y.")
  }

  copY <- (Mx / Fz)*100
  return(copY)
}
