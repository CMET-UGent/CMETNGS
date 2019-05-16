#' Function to process mothur generated alpha-diversity collector curves
#'
#' This (legacy) function allows users to visualize mothur-generated collector
#' curve data using R.
#'
#' @param fnpref file prefix string for the collector curve data
#' @param groups (ordered) character vector with names of the samples
#' @param fnpost name of calculator used (character)
#' @param errorbars logical indicating whether or not error bars should be plotted
#'  at this stage, the code does NOT check whether or not the data for the error
#'  bars is available so it is the users responsibility to make sure it is.
#' @param printing logical to indicate whether or not the plot should be displayed
#' @importFrom ggplot2 ggplot geom_line ylab geom_errorbar aes
#' @importFrom utils read.table
#' @keywords mothur, collector curves
#' @return The underlying dataframe for the plot and the plot (if printing is set to TRUE)
#' @examples
#' #TODO
#' @export

colcurveg <- function(fnpref,groups,fnpost,errorbars=FALSE,printing=FALSE)
{
  calcdfcomplete<-data.frame()
  for(i in 1:length(groups))
  {
    j<-groups[i]
    tmpsmp<-read.table(paste(fnpref,j,".",fnpost,sep=""),header=TRUE)
    repcount<-nrow(tmpsmp)
    calcdfcomplete.tmp <- cbind(tmpsmp,rep(j,repcount))
    calcdfcomplete <- rbind(calcdfcomplete,calcdfcomplete.tmp)
  }
  names(calcdfcomplete)[ncol(calcdfcomplete)]<-"Sample"
  calcgg <- ggplot(aes(x=numsampled,y=X0.03,color=Sample),data=calcdfcomplete) +
    geom_line() + ylab(paste(fnpost," index",sep=""))
  if(errorbars==TRUE)
  {
    calcggerr <- calcgg + geom_errorbar(aes(ymin=lci,ymax=hci))
    if(printing==TRUE){print(calcggerr)}
    reslist <- list(plotgg=calcggerr,underlyingdf=calcdfcomplete)
  }else{
    if(printing==TRUE){print(calcgg)}
    reslist <- list(plotgg=calcgg,underlyingdf=calcdfcomplete)
  }
  return(reslist)
}
