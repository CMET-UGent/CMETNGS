#' Parsing function for consensus as found by sangeranalyseR::merge.reads
#'
#' @param x sangeranalyseR::merged.read class object
#' @importFrom assertthat has_name
#' @importFrom BiocGenerics Reduce
#' @examples
#' ## Simple example
#'
#' @export

getconsensus <- function(x){
  # sanity check: does the merged.read object have a consensus?
  if(!has_name(x,"consensus")){
    stop(date()," --- no consensus read could be found in the merged read object. \n")
  }
  # extract consensus slot
  conschar <- as.character(x$consensus)
  identifiernames <- sub("(.*)\\..*","\\1",names(x$alignment))
  identifier <- paste(Reduce(intersect2,
                             strsplit(identifiernames[1:2],NULL)),collapse="")
  names(conschar) <- identifier
  return(conschar)
}
