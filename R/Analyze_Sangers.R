#' Preprocessing and annotation of Sanger sequencing of the 16S rRNA gene
#'
#' @param inputfile LGC text file containing forward and reversed reads
#' @param inputfolder folder containing the ab1 files with the raw spectrograms
#'  from an ABI capillary electrophoresis instrument. only used when raw=TRUE
#' @param fwd_suffix string containing the suffix of the forward reads
#'  (defaults to F)
#' @param rev_suffix string containing the suffix of the reverse reads
#'  (defaults to R)
#' @param fastas_only if you do not want merging to occur but want separate
#'  fasta files to be spitted out for downstream processing in e.g. BioEdit. (
#'  defaults to FALSE)
#' @param resfolder name of the folder to put the results in (defaults to
#'  "Results/")
#' @param verbose print out progress (defaults to FALSE)
#' @param trim.cutoff sangeranalyseR::summarise.abi.folder argument (defaults to 5e-3)
#' @param mintrim minimal length of trimmed reads in the folder workflow to
#'  retain before merging (defaults to 300)
#' @param ... additional arguments passed on to BioStrings::readDNAStringSet or
#'  sangeranalyseR::summarise.abi.folder in case inputfile==NULL and inputfolder
#'  is specified
#' @param ncores number of cores for sangeranalyseR::summarise.abi.folder (
#'  defaults to 2, and will be set to 1 on windows)
#' @importFrom Biostrings readDNAStringSet reverseComplement
#' @importFrom sangeranalyseR merge.reads summarise.abi.folder
#' @importFrom dplyr filter inner_join
#' @importFrom tidyr %>%
#' @importFrom utils capture.output
#' @return either a list of the consensus reads, forward and reverse reads or a summarised abi folder
#' @examples
#' ##File-based workflow (LGC-preprocessed)
#'
# fastademoset <- system.file("extdata","sanger_demodata.txt",
#                               package = "CMETNGS",mustWork = TRUE)
# consensusres   <- Analyze_Sangers(inputfile=fastademoset,fwd_suffix="F",
#                                   rev_suffix="R",verbose=TRUE)
#
# ##File-based workflow for DSP in BioEdit
#
# fastares   <- Analyze_Sangers(inputfile=fastademoset,fwd_suffix="F",
#                                rev_suffix="R",fastas_only=TRUE)
#' ##Folder-based workflow
#'
#' # TODO: find proper example set
#' @export

Analyze_Sangers <- function(inputfile=NULL,inputfolder=NULL,
                            fwd_suffix="F",rev_suffix="R",fastas_only=FALSE,
                            resfolder="Results",verbose=FALSE,trim.cutoff=5e-3,
                            ncores=2,mintrim=300L,
                            ...){
  if(.Platform$OS.type == "windows") {
    ncores <- 1 # parallelization not permitted on windows just yet
  }
  if(!dir.exists(file.path(resfolder))){
    dir.create(file.path(resfolder))}
  if(!is.null(inputfile)){
    if(!is.null(inputfolder)){stop(date(),
                              " --- you need to supply either an inputfile or",
                              "a folder, but cannot supply both.\n")}
    if(verbose){cat(date()," --- Starting readout of",inputfile,".\n")}
    SangerDataSet <- readDNAStringSet(filepath=inputfile,format="fasta",
                                      nrec=-1L,skip=0L,seek.first.rec=FALSE,
                                      use.names=TRUE,...)
    if(verbose){cat(date()," --- Succesfully parsed",length(SangerDataSet),
                    "reads.\n")}
    # additional sanity check: is there a match with the current suffix or not?

    # actually extract fwd/rev
    revs    <- SangerDataSet[grep(paste0("^.*",rev_suffix,"\\..*$"),
                               names(SangerDataSet))]
    revs.rc <- reverseComplement(revs)
    fwds    <- SangerDataSet[grep(paste0("^.*",fwd_suffix,"\\..*$"),
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
    cons <- sapply(X=merged.reads,FUN=getconsensus)
    writeXStringSet(DNAStringSet(cons),file.path(resfolder,"consensus.fasta"))
    reslist <- list(forward=fwd.s,reversecomplemented=revs.rc.s,concensus=cons)
    } else {
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

      allseqs <- union(fwds.s,revs.rc.s)
      allnames <- names(allseqs)
      identifiernames <- sub("(.*)\\..*","\\1",allnames)
      uniqueidnames <- unique(sub(paste0("(^.*)",fwd_suffix,"$"),"\\1",
                                  sub(paste0("(^.*)",rev_suffix,"$"),"\\1",
                                  identifiernames)))
      for(i in uniqueidnames){
          namessel <- grep(i,names(allseqs))
          subsetsel <- allseqs[namessel]
          writeXStringSet(subsetsel,filepath = file.path(resfolder,
                                                         paste0(gsub("\\.","\\_",i),
                                                                ".fasta")),
                          format = "fasta",append=FALSE)
      }
      reslist <- list(fwd_reads=fwds.s,rev_reads_complement=revs.rc.s,
                      uniqueids=uniqueidnames)
      if(verbose){cat(date()," --- Analyze_sangers finished, wrote",
                      length(fwds.s)+length(revs.rc.s)+length(uniqueidnames),
                      "files.\n")}
    }
  }else{
    if(is.null(inputfolder)){stop(date(),
                                   " --- you need to supply either an inputfile or",
                                   "a folder. \n")}
    sfabifld <- summarise.abi.folder(input.folder = inputfolder,
                                     trim.cutoff = trim.cutoff,
                                     processors=ncores,
                                     ...)
    longseq <- sfabifld$summaries[sfabifld$summaries$trimmed.length>mintrim,]

    strpnames <- sub("(^.*)\\..*\\.ab1$","\\1",longseq$file.name)
    strpnamesnoFR <- sub(paste0("(^.*)",fwd_suffix,"$"),"\\1",
                         sub(paste0("(^.*)",rev_suffix,"$"),"\\1",strpnames))
    longseq <- data.frame(longseq,
                          Sample=strpnamesnoFR)
    longseq.fwd <- longseq %>% dplyr::filter(grepl(paste0("^.*",
                                            fwd_suffix,"$"),strpnames)) %>%
                               droplevels()
    longseq.rev <- longseq %>% dplyr::filter(grepl(paste0("^.*",
                                              rev_suffix,"$"),strpnames)) %>%
      droplevels()
    colnames(longseq.fwd)[1:ncol(longseq.fwd)-1] <-
      paste0("fwd",colnames(longseq.fwd)[1:ncol(longseq.fwd)-1])
    colnames(longseq.rev)[1:ncol(longseq.rev)-1] <-
      paste0("rev",colnames(longseq.rev)[1:ncol(longseq.rev)-1])

    matchingseq <- inner_join(longseq.fwd,longseq.rev,by="Sample")
    filetable <- data.frame(fwdfile=matchingseq$fwdfile.path,
                            revfile=matchingseq$revfile.path,
                            samplename=matchingseq$Sample)

    if(!fastas_only){
      #write out individual consensus files
      reslist <- list()
      for(i in 1:nrow(filetable)){
        fwdfil <- as.character(filetable[i,1])
        revfil <- as.character(filetable[i,2])
        smpnam <- as.character(filetable[i,3])
        invisible(capture.output(reslist[[i]]<-
                                   readmerger(fwdfile=fwdfil,
                                              revfile=revfil,
                                              samplename=smpnam)))
      }

      for(i in 1:nrow(filetable)){
        fwdfil <- as.character(filetable[i,1])
        revfil <- as.character(filetable[i,2])
        smpnam <- as.character(filetable[i,3])
        invisible(capture.output(reslist[[i]]<- readmerger(fwdfile=fwdfil,
                                                           revfile=revfil,
                                                           samplename=smpnam,
                                                           onefile=TRUE)))
      }
      resdf <- do.call(rbind,lapply(reslist,function(x)as.data.frame(x)))

    } else {
      stop(date()," --- Currently, export of non-consensus abi fastas is not",
           "implemented yet - check the returned list for more details")
    }
    reslist <- ssfabifld
    return(reslist)

  }

}
