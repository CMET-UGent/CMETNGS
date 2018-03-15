#' function to generate ggplot-like color pallettes
#'
#' @param n number of colors
#' @importFrom grDevices hcl
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find test object
#'
#' @export


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
