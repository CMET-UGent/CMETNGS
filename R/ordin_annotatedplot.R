#' Create an annotated ordination graph
#'
#' This function is a wrapper around CMETNGS::ordin_annotate and CMETNGS::ordin_legend:
#' it uses a vegan ordination object (e.g. from metaMDS) and a valid metadata
#' table with Factor1 and Factor2 to create an annotated plot.
#'
#' @param ordinobj a vegan-created ordination object (i.e. ordinobj$points must
#'  exist)
#' @param metadata a valid CMET metadata object, containing minimally the sample
#'  names, Factor1 and Factor2.
#' @param location where to put the legend on the plot, defaults to "bottomleft"
#' @param addLabels wether or not to add sample labels to the ordination (
#'  defaults to TRUE)
#' @param addLegend a logical, indicating wether or not to add a legend to the
#'  plot. Defaults to TRUE.
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics legend points
#' @importFrom vegan ordiplot ordipointlabel metaMDS
#' @keywords ordination
#' @return A list with the colvector and pchvector
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' library(vegan)
#' data(dune)
#' dune.dis <- vegdist(wisconsin(dune))
#' dune.mds <- cmdscale(dune.dis, eig = TRUE)
#' #create completely made-up metadata for dune
#' dune.md <- data.frame(Factor1=c(rep("Dune1",nrow(dune)/2),
#' rep("Dune2",nrow(dune)/2)),Factor2=c(rep("Grass",7),rep("Shrub",7),
#' rep("Tree",6)))
#' ordin_annotatedplot(dune.mds,dune.md)
#' @export

ordin_annotatedplot <- function(ordinobj,metadata,location="bottomleft",
                                addLabels=TRUE,addLegend=TRUE)
{
  # subset/reorder metadata ----------------------------------------------------
  md.ordinobj <- metadata[rownames(ordinobj$points),]
  # Prepare color vector based upon Factor 1 -----------------------------------
  brewset <- RColorBrewer::brewer.pal(9,"Set1")
  # this means that if factor 1 has more than 9 levels, we cannot use coloring
  # anymore. In that case we will leave all points blue.
  colvect<-rep(brewset[1],length(rownames(ordinobj$points)))
  if(nlevels(factor(md.ordinobj$Factor1))<=length(brewset)){
    for(i in 2:nlevels(factor(md.ordinobj$Factor1))){
      colvect[which(md.ordinobj$Factor1==
                      levels(factor(md.ordinobj$Factor1))[i])]<- brewset[i]
    }
  }else{
    warning(paste("You supplied a metadata factor with more levels",
                  nlevels(factor(md.ordinobj$Factor1)),"than currently",
                  "supported by this function (9). \n","Hence, only one",
                  "color will be used in the ordination plot"))
  }
  # Prepare shape vector based upon Factor 2 -------------------------------------
  pchset <- 15:19
  pchvect <- rep(pchset[1],length(rownames(ordinobj$points)))
  # only completely filled shapes are used by default (i.e. 15:19), which means
  # only five levels are tolerated for Factor 2, otherwise all remains in single
  # pch type (square)
  if(nlevels(factor(md.ordinobj$Factor2))<=5){
    for(i in 2:nlevels(factor(md.ordinobj$Factor2))){
      pchvect[which(md.ordinobj$Factor2==
                      levels(factor(md.ordinobj$Factor2))[i])]<- pchset[i]
    }
  }else{
    warning(paste("You supplied a metadata factor with more levels",
                  nlevels(factor(md.ordinobj$Factor2)),"than currently",
                  "supported by this function (5). \n","Hence, only one",
                  "shape will be used in the ordination plot"))
  }
  ordiplot(ordinobj,type="n",display="sites")
  if(addLabels){ordipointlabel(ordinobj,display = "sites",add=TRUE)}
  if(sum(grepl("metaMDS",class(ordinobj)))){
    points(ordinobj,pch=pchvect,col=colvect)
  } else {
    if(is.list(ordinobj)){
      x <- ordinobj$points[,1]
      y <- ordinobj$points[,2]
      points(x,y,pch=pchvect,col=colvect)
    }else{
      x <- ordinobj[,1]
      y <- ordinobj[,2]
      points(x,y,pch=pchvect,col=colvect)
    }
  }

  if(addLegend){
    if(nlevels(factor(metadata$Factor1))<=length(brewset)){
      if(nlevels(factor(metadata$Factor2))<=5){
        legdf <- data.frame(expand.grid(brewset[1:nlevels(factor(metadata$Factor1))],
                                        pchset[1:nlevels(factor(metadata$Factor2))]),
                            label=do.call(paste,
                                          expand.grid(levels(metadata$Factor1),
                                                      levels(metadata$Factor2))))
        legend(location,
               legend = legdf$label,
               col = as.character(legdf$Var1),
               pch = legdf$Var2)
      } else {
        legend(location,
               legend = levels(factor(metadata$Factor1)),
               col=brewset[1:nlevels(factor(metadata$Factor1))],
               pch = pchset[1])
      }
    } else {
      if(nlevels(factor(metadata$Factor2))<=5){
        legend(location,
               legend = levels(factor(metadata$Factor2)),
               col= brewset[1],
               pch = pchset[1:nlevels(factor(metadata$Factor2))])
      }
    }
  }
}
