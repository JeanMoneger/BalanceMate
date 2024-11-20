#' CoPX_ComputeR
#' @description
#' A function to compute Center of pressure (CoP) values on the medio-lateral axis (X-axis). The function only requires moments of forces on the Y-axis and Force on the Z-axis which are provided in most postural tools outputs.
#'
#' @param My numeric vector specifying the moment on the Y-axis
#' @param Fz numeric vector specifying the force on the Z-axis
#'
#' @return A numeric vector containing the Center of pressure values (cm) on the X-axis
#' @export
#'
#' @seealso [CoPY_ComputeR()] for computing CoP-Y values
#'
#' @examples path_to_data <- system.file("extdata", package = "BalanceMate")
#' Data <- Merge_PosData(path_to_data, SampleRate = 100, SessionDuration = 100)
#'
#' CoPX_ComputeR(Data$My, Data$Fz)
CoPX_ComputeR <- function(My, Fz) {
  # Error handling for division by zero
  if (any(Fz == 0)) {
    stop("Fz cannot be zero to compute CoP X.")
  }

  copX <- (-My / Fz)*100
  return(copX)
}
