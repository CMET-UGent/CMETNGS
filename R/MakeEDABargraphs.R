#' Function to automatically generate a few Phyloseq bargraphs on the data
#'
#' This function uses a shared file (i.e. a mothur-formatted OTU contingency
#' table) and a taxonomy file (i.e. an RDP naive bayesian fixed-rank
#' asignment detail) to generate three stacked barcharts in the folder it is
#' executed in. The function returns a phyloseq object as well by default.
#'
#' @param shared file location string for the shared file
#'  (preferably absolute path)
#' @param taxonomy file location string for the taxonomy file (preferably
#'  absolute path)
#' @param retfphyloseq wheter or not to return a phyloseq object (defaults
#'  to true)
#' @importFrom phyloseq import_mothur sample_names sample_sums taxa_sums merge_phyloseq tax_table prune_taxa transform_sample_counts
#' @importFrom tidyr %>%
#' @importFrom ggplot2 theme guides element_text guide_legend ggsave
#' @keywords EDA stacked bargraph, mothur
#' @return A phyloseq object and three plots in the current directory
#' @examples
#'
#' shareddataset <- system.file("extdata","large_shared_file.shared",
#'                               package = "CMETNGS",mustWork = TRUE)
#' taxonomydataset <- system.file("extdata","large_OTU_basedtax.taxonomy",
#'                               package = "CMETNGS",mustWork = TRUE)
#' edagraphs <- MakeEDABargraphs(shareddataset,taxonomydataset)
#'
#'
#' @export

MakeEDABargraphs <- function(shared,taxonomy,retfphyloseq=TRUE){
  ### Sanity checks ####
  if(is.null(shared)|!is.character(shared)|length(shared)!=1|
     !file.exists(shared)){
    stop("You did not provide a valid shared file location")
  }
  if(is.null(taxonomy)|!is.character(taxonomy)|length(taxonomy)!=1|
     !file.exists(taxonomy)){
    stop("You did not provide a valid taxonomy file location")
  }
  #### read files ####
  mothur_data <- phyloseq::import_mothur(mothur_shared_file = shared,
                               mothur_constaxonomy_file = taxonomy)
  colnames(phyloseq::tax_table(mothur_data)) <- c("Kingdom", "Phylum", "Class",
                                       "Order", "Family", "Genus")
  phylobj <- mothur_data
  # Make a data frame with a column for the read counts of each sample
  sample_sum_df <- data.frame(sum = phyloseq::sample_sums(phylobj))
  #Select top OTUs
  TopNOTUs <- names(sort(phyloseq::taxa_sums(phylobj), TRUE)[1:25])
  physeqobj25  <- phyloseq::prune_taxa(TopNOTUs, phylobj)
  relabsphyseq <- phyloseq::transform_sample_counts(phylobj, function(x) x/sum(x))
  TopNOTUs.relab <- names(sort(phyloseq::taxa_sums(relabsphyseq), TRUE)[1:25])
  physeqobj25.relab <- phyloseq::prune_taxa(TopNOTUs.relab, relabsphyseq)

  AbsAbunGen <- phyloseq::plot_bar(physeqobj25, fill="Genus",
                                   title="Absolute abundance top 25 OTU's") +
                          ggplot2::guides(fill=guide_legend(ncol=1)) +
                          ggplot2::theme(axis.text.x=
                                           ggplot2::element_text(angle=90,
                                                         hjust=0.95,
                                                         vjust=0.3))
  ggplot2::ggsave("AbsAbunGen.png",plot = AbsAbunGen,device="png")

  RelAbunGen  <- phyloseq::plot_bar(physeqobj25, fill="Genus",
                                   title="Relative  abundance top 25 OTU's") +
                          ggplot2::guides(fill=guide_legend(ncol=1)) +
                          ggplot2::theme(axis.text.x=
                                           ggplot2::element_text(angle=90,
                                           hjust=0.95,
                                           vjust=0.3))
  ggplot2::ggsave("RelAbunGen.png",plot = RelAbunGen,device="png")

  RelAbunFam  <- phyloseq::plot_bar(physeqobj25, fill="Family",
                                    title="Relative abundance top 25 OTU's") +
                            ggplot2::guides(fill=guide_legend(ncol=1)) +
                            ggplot2::theme(axis.text.x=
                                             ggplot2::element_text(angle=90,
                                           hjust=0.95,
                                           vjust=0.3))
  ggplot2::ggsave("RelAbunFam.png",plot = RelAbunFam,device="png")

  if(retfphyloseq==TRUE){
    return(phylobj)
  }

}
