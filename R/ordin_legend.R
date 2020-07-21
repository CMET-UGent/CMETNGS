#' Create legend for base vegan ordination plots
#'
#' This function uses a vegan ordination object (e.g. from metaMDS) and
#' a valid metadata table with Factor1 and Factor2 and is meant to be used in
#' conjunction with CMETNGS::ordin_annotate.
#'
#' @param metadata a valid CMET metadata object, containing minimally the sample
#'  names, Factor1 and Factor2.
#' @param location where to put the legend on the plot, defaults to "bottomleft"
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics legend
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
#' ordipointlabel(dune.mds,display = "sites",add=TRUE)
#' ordin_legend(dune.md,location="topright")
#' @export

ordin_legend <- function(metadata,location="bottomleft")
{
  brewset <- brewer.pal(9,"Set1")  # this is fixed (for now)
  pchset <- 15:19                  # this is fixed (for now)
  if(nlevels(factor(metadata$Factor1))<=length(brewset)){
    if(nlevels(factor(metadata$Factor2))<=5){
      legdf <- data.frame(expand.grid(brewset[1:nlevels(factor(metadata$Factor1))],
                                    pchset[1:nlevels(factor(metadata$Factor2))]),
                        label=do.call(paste,expand.grid(levels(factor(metadata$Factor1)),
                                    levels(factor(metadata$Factor2)))))
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
