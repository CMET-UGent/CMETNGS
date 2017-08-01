#' function to draw a circle
#'
#' This function draws a circle with given parameters
#'
#' @param center vector of length 2 with coordinates for the center (dfaults to c(0,0))
#' @param diameter numeric indicating diamter (defaults to 1)
#' @param npoints number of points (defaults to 100)
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find test object
#'
#' @export

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
