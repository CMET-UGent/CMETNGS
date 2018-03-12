#' Alternative rounding function
#'
#' @param x numeric vector to be rounded
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find test object
#'
#' @export

myround <- function(x){trunc(x+0.5)}
