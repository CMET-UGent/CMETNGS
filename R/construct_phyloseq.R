#' Function to automatically generate a phyloseq object from mothur output
#'
#' This function uses a shared file (i.e. a mothur-formatted OTU contingency
#' table),a taxonomy file (i.e. an RDP naive bayesian fixed-rank
#' asignment detail) and an optional fasta file with representative sequences
#' for every OTU as well as an optional metadata file to be merged into a
#' phyloseq object.
#'
#' @param shared file location string for the shared file
#'  (preferably absolute path)
#' @param taxonomy file location string for the taxonomy file (preferably
#'  absolute path)
#' @param otureps optional file location for the respresentative fasta file (
#'  preferably absolute path)
#' @param metadata optional file location for the metadata.xlsx file (preferably
#'  absolute path)
#' @param remove_singletons boolean (defaults to TRUE) indicating wether or not
#'  absolute singletonds should be removed
#' @param dataname string indicating the name of data, will be used if the sample
#'  names are completely numerical as a prefix (default: "phydata")
#' @importFrom data.table fread
#' @importFrom tidyr %>%
#' @importFrom dplyr select contains inner_join
#' @importFrom phyloseq otu_table sample_data tax_table refseq merge_phyloseq phyloseq taxa_names
#' @importFrom Biostrings readDNAStringSet
#' @importFrom readxl read_xlsx
#' @keywords phyloseq, mothur
#' @return A phyloseq object
#' @examples
#' ## Example without otureps
#' # make sure library(CMETNGS) is loaded
# shareddataset <- system.file("extdata","large_shared_file.shared",
#                               package = "CMETNGS",mustWork = TRUE)
# taxonomydataset <- system.file("extdata","large_OTU_basedtax.taxonomy",
#                               package = "CMETNGS",mustWork = TRUE)
# phyobj1 <- construct_phyloseq(shareddataset,taxonomydataset)
#'
#' ## Example with otureps
# oturepsdataset <- system.file("extdata","large_otureps.fasta",
#                               package="CMETNGS",mustWork=TRUE)
# phyobj1wreps <- construct_phyloseq(shareddataset,taxonomydataset,
#                                    otureps=oturepsdataset)
#'
#' @export

construct_phyloseq <- function(shared,taxonomy,otureps=NULL,metadata=NULL,
                               remove_singletons=TRUE,dataname="phydata"){
  #### Sanity checks ####
  if(is.null(shared)|!is.character(shared)|length(shared)!=1|
     !file.exists(shared)){
    stop("You did not provide a valid shared file location")
  }
  if(is.null(taxonomy)|!is.character(taxonomy)|length(taxonomy)!=1|
     !file.exists(taxonomy)){
    stop("You did not provide a valid taxonomy file location")
  }

  #### read files ####
  shared <- fread(input = shared,
                  header = TRUE)
  taxonomy <- fread(input = taxonomy)

  #### preformat the data ####
  samplenames <- shared$Group
  shared.t <- t(shared[,4:ncol(shared)])
  colnames(shared.t) <- samplenames

  taxonomy.clean <- CMETNGS::preformattax(taxonomy = taxonomy,
                                          keepOTU = TRUE)
  taxonomy.clean <- taxonomy.clean %>% dplyr::select(-contains("Prob"))

  if(remove_singletons){
    shared.t.ns <- shared.t[which(rowSums(shared.t)!=1),]
    taxonomy.clean <- taxonomy.clean[which(rownames(taxonomy.clean)
                                        %in% rownames(shared.t.ns)),]
  }else{
    shared.t.ns <- shared.t
  }
  shared.t.ns.otu <- data.frame(OTU=rownames(shared.t.ns),shared.t.ns)

  if(!is.null(metadata)){
    if(!is.character(metadata)|length(metadata)!=1|
       !file.exists(metadata)){
      stop("You did not provide a valid metadata file location")
    }
    metadata.tibble <- readxl::read_excel(metadata,sheet="ForR")
    factdescs <- readxl::read_excel(metadata,sheet="FactDesc")
    metadata <- as.data.frame(metadata.tibble)
    # to avoid warnings/errors with rownames
    if(is.numeric(metadata$SampleName)) #check for fully numeric sample names
    {
      metadata$SampleName <- paste(dataname,metadata$SampleName,sep="")
    }
    rownames(metadata) <- metadata$SampleName
    metadata.smpdat <- sample_data(metadata)
    colnames(shared.t.ns) <- plyr::mapvalues(colnames(shared.t.ns),
                                             from=as.character(metadata$Code),
                                             to=as.character(metadata$SampleName))
  }
  otumat.ns <- as.matrix(shared.t.ns)
  taxmat.ns <- as.matrix(taxonomy.clean)
  OTU       <- otu_table(otumat.ns,taxa_are_rows = TRUE)
  TAX       <- tax_table(taxmat.ns)
  physeqobj <- phyloseq(OTU,TAX)
  if(!is.null(metadata)){
    physeqobj <- merge_phyloseq(physeqobj,metadata.smpdat)
  }
  if(!is.null(otureps)){
    if(!is.character(otureps)|length(otureps)!=1|
       !file.exists(otureps)){
      stop("You did not provide a valid file location for the representative sequences of the OTUs")
    }
    oturepsreads <- Biostrings::readDNAStringSet(otureps)
    names(oturepsreads) <- sub(".+\\t(Otu[0-9]+)\\|.*","\\1",
                               names(oturepsreads))
    if(remove_singletons){
      oturepsreads <- oturepsreads[names(oturepsreads) %in% taxa_names(physeqobj)]
    }
    physeqobj <- merge_phyloseq(physeqobj,oturepsreads)
  }
  return(physeqobj)
}
