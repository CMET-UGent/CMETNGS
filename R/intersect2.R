#' Intersection function for two character vectors
#'
#' @param x character vector x
#' @param y character vector y
#' @importFrom BiocGenerics match
#' @examples
#' ## Simple example
#'
#' @export

intersect2 <- function (x, y)
{
  y <- as.vector(y)
  y[match(as.vector(x), y, 0L)]
}
