#' Function to automatically generate the Excel-export OTU table
#'
#' This function uses a shared file (i.e. a mothur-formatted OTU contingency
#' table),a taxonomy file (i.e. an RDP naive bayesian fixed-rank
#' asignment detail) and an optional fasta file with representative sequences
#' for every OTU as well as an optional metadata file to be merged into the
#' CMET style "Excel-report". Currently the metadata file is not being made use
#' of and the option to use multiple taxonomy references is also to be
#' implemented at a later date.
#'
#' @param shared file location string for the shared file
#'  (preferably absolute path)
#' @param taxonomy file location string for the taxonomy file (preferably
#'  absolute path)
#' @param oturep optional file location for the respresentative fasta file (
#'  preferably absolute path)
#' @param metadata optional file location for the metadata.xlsx file (preferably
#'  absolute path)
#' @param resultfn desired filename/path (absolute) for the resulting Excel
#'  report. Defaults to Results.xlsx in the current working directory.
#' @importFrom data.table fread
#' @importFrom tidyr %>%
#' @importFrom dplyr select contains inner_join
#' @importFrom openxlsx write.xlsx
#' @keywords excel report generator, mothur
#' @return A dataframe with the contents of the excel file that was written
#' @examples
#' ## Example without otureps
#'
#' shareddataset <- system.file("extdata","large_shared_file.shared",
#'                               package = "CMETNGS",mustWork = TRUE)
#' taxonomydataset <- system.file("extdata","large_OTU_basedtax.taxonomy",
#'                               package = "CMETNGS",mustWork = TRUE)
#'
#' ## Eaxample with otureps
#'
#' @export

MakeExcelReport <- function(shared,taxonomy,otureps=NULL,metadata=NULL,
                            resultfn="Results.xlsx"){
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

  shared.t.ns <- shared.t[which(rowSums(shared.t)!=1),]
  shared.t.ns.otu <- data.frame(OTU=rownames(shared.t.ns),shared.t.ns)

  taxonomy.clean <- CMETNGS::preformattax(taxonomy = taxonomy,keepOTU = TRUE)
  taxonomy.clean <- taxonomy.clean %>% dplyr::select(-contains("Prob"))

  #### merge dfs ####
  mergedf <- dplyr::inner_join(shared.t.ns.otu,taxonomy.clean,by="OTU")

  if(!is.null(otureps)){
    if(!is.character(otureps)|
       length(otureps)!=1|
       !file.exists(otureps)){
      warning("You supplied an invalid otureps file location. Therefore the
              resulting report will not contain any representative sequences")
    }else{
      otureps <- CMETNGS::fasta2dataframe(otureps)
      names(otureps)[1] <- "OTU"
      mergedfreps <- dplyr::inner_join(mergedf,otureps,by="OTU")
      openxlsx::write.xlsx(mergedfreps,file=resultfn)
      return(mergedfreps)
    }
  }
  #### write to excel ####
  openxlsx::write.xlsx(mergedf,file=resultfn)
  return(mergedf)
}
