#' function to combine unequal-length data frames (or vectors) into a data.frame
#'
#' This function combines unequal-length data frames (or vectors) into a data.frame
#' It is copied from https://stackoverflow.com/questions/14899306/transform-a-splitted-data-frame-in-a-new-data-frame
#' @param ... vectors or data frames of unequal length
#' @param pad what to pad the resulting data frame with (defaults to NA)
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find test object
#'
#' @export

cbindPad <- function(...,pad=NA){
  args <- list(...)
  n <- sapply(args,nrow)
  mx <- max(n)
  pad <- function(x, mx){
    if (nrow(x) < mx){
      nms <- colnames(x)
      padTemp <- matrix(pad, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x)==0) {
        return(padTemp)
      } else {
        return(rbind(x,padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args,pad,mx)
  return(do.call(cbind,rs))
}
