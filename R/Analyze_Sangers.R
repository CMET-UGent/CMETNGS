#' Preprocessing and annotation of Sanger sequencing of the 16S rRNA gene
#'
#' @param inputfile LGC text file containing forward and reversed reads
#' @param inputfolder folder containing the ab1 files with the raw spectrograms
#'  from an ABI capillary electrophoresis instrument. only used when raw=TRUE
#' @param fwd_suffix string containing the suffix of the forward reads
#'  (defaults to f)
#' @param rev_suffix string containing the suffix of the reverse reads
#'  (defaults to r)
#' @param fastas_only if you do not want merging to occur but want separate
#'  fasta files to be spitted out for downstream processing in e.g. BioEdit. (
#'  defaults to FALSE)
#' @param resfolder name of the folder to put the results in (defaults to
#'  "Results/")
#' @param verbose print out progress (defaults to FALSE)
#' @param ... additional arguments passed on to BioStrings::readDNAStringSet
#' @importFrom Biostrings readDNAStringSet reverseComplement
#' @importFrom sangeranalyseR merge.reads
#' @examples
#' ## File-based workflow (LGC-preprocessed)
#'
#' fastademoset <- system.file("extdata","sanger_demodata.txt",
#'                               package = "CMETNGS",mustWork = TRUE)
#' fastalnres   <- Analyze_sangers(inputfile=fastademoset)
#'
#' @export

Analyze_Sangers <- function(inputfile=NULL,folder=NULL,
                            fwd_suffix="f",rev_suffix="r",fastas_only=FALSE,
                            resfolder="Results",verbose=FALSE,
                            ...){
  if(!dir.exists(file.path(resfolder))){
    dir.create(file.path(resfolder))}
  if(!is.null(inputfile)){
    if(!is.null(folder)){stop(data(),
                              " --- you need to supply either an inputfile or",
                              "a folder, but cannot supply both.\n")}
    if(verbose){cat(date()," --- Starting readout of",inputfile,".\n")}
    SangerDataSet <- readDNAStringSet(filepath=inputfile,format="fasta",
                                      nrec=-1L,skip=0L,seek.first.rec=FALSE,
                                      use.names=TRUE,...)
    if(verbose){cat(date()," --- Succesfully parsed",length(SangerDataSet),
                    "reads.\n")}
    revs    <- SangerDataSet[grep(paste0("^.*",rev_suffix,"$"),
                               names(SangerDataSet))]
    revs.rc <- reverseComplement(revs)
    fwds    <- SangerDataSet[grep(paste0("^.*",fwd_suffix,"$"),
                               names(SangerDataSet))]
    # sanity check: is there an equal number of forward and reverse reads?
    if(!(length(revs.rc)==length(fwds))){
      stop(date()," --- The number of forward reads (",length(fwds),
           ") did not match the number of reverse reads (",length(revs.rc),
           "). Please correct. \n",call. = FALSE)
    }
    # make sure the fwd and rev reads are sorted in order
    fwds.s    <- fwds[sort(names(fwds))]
    revs.rc.s <- revs.rc[sort(names(revs.rc))]
    if(!fastas_only){
    # merge the reads
    merged.reads <- list()
    for(i in 1:length(fwds.s)){
      if(verbose){cat(date()," --- merging reads",names(fwds.s)[i],
                      "and",names(revs.rc.s)[i],".\n")}
      reads <- c(fwds.s[i],revs.rc.s[i])
      merged.reads[[i]] <-merge.reads(reads)
    }
    if(verbose){cat(date()," --- Merged ",length(fwds.s),"reads.\n")}
    } else {
      # TODO: only use common part of the name to group F&R into
      # separate Xstringset for writing for easier DSP
      for(i in names(fwds.s)){
      writeXStringSet(fwds.s[i],filepath = file.path(resfolder,
                                                     paste0(gsub("\\.","\\_",i),
                                                            ".fasta")),
                      format = "fasta",append=FALSE)
      }
      for(i in names(revs.rc.s)){
        writeXStringSet(revs.rc.s[i],filepath = file.path(resfolder,
                                                    paste0(gsub("\\.","\\_",i),
                                                              "_revC.fasta")),
                        format = "fasta",append=FALSE)
      }
      if(verbose){cat(date()," --- Analyze_sangers finished, wrote",length(fwds.s)+length(revs.rc.s),"files.\n")}
    }
  }else{

  }

}
