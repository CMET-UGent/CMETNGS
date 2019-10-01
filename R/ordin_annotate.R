#' Create color and shape annotation for base vegan ordination plots
#'
#' This function uses a vegan ordination object (e.g. from metaMDS) and
#' a valid metadata table with Factor1 and Factor2 and returns a list with
#' colvect and pchvect. The colvect supports at maximum 9 levels of Factor 1 (
#' otherwise only 1 color (#E41A1C) is returned). The pchvect supports at
#' maximum 5 levels of Factor 1 (otherwise only 1 shape, 15 (filled square)) is
#' retured.
#'
#' @param ordinobj a vegan-created ordination object (i.e. ordinobj$points must
#'  exist)
#' @param metadata a valid CMET metadata object, containing minimally the sample
#'  names, Factor1 and Factor2.
#' @importFrom RColorBrewer brewer.pal
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
#' veclist <- ordin_annotate(dune.mds,dune.md)
#' dune.mds$species <- wascores(dune.mds$points, dune, expand = TRUE)
#' pl <- ordiplot(dune.mds, type = "none")
#' points(pl, "sites", pch=veclist$pchvect, col=veclist$colvect, bg="yellow")
#' @export

ordin_annotate <- function(ordinobj,metadata)
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
    warning(paste("You supplied a metadata factor with more levels (",
                  nlevels(factor(md.ordinobj$Factor1)),") than currently",
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
    warning(paste("You supplied a metadata factor with more levels (",
                  nlevels(factor(md.ordinobj$Factor2)),") than currently",
                  "supported by this function (5). \n","Hence, only one",
                  "shape will be used in the ordination plot"))
  }
  # create and return list of results
  reslist <- list(colvect=colvect,pchvect=pchvect)
  return(reslist)
}
