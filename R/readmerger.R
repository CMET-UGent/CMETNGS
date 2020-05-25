#' Support function for merging abifldr objects primary sequences
#'
#' @param fwdfile path to forward ab1 file
#' @param revfile path to reverse ab1 file
#' @param outputfolder path to output folder
#' @param samplename name for fasta identifier/file name for merged read
#' @param onefile whether or not to write out one fasta file with all consensus
#'  reads (defaults to FALSE)
#' @importFrom Biostrings reverseComplement DNAStringSet
#' @importFrom sangerseqR readsangerseq primarySeq
#' @importFrom sangeranalyseR merge.reads
#' @importFrom ape write.dna
#' @return a list containing the sample, consensus length and consensus seq
#' @examples
#' ##Simple example
#'
#' @export

readmerger <- function(fwdfile,revfile,samplename,onefile=FALSE,
                       outputfolder="Results"){
  if(!dir.exists(file.path(outputfolder))){
    dir.create(file.path(outputfolder))}
  fwd <- readsangerseq(fwdfile)
  rev <- readsangerseq(revfile)
  fwd <- primarySeq(fwd)
  rev <- primarySeq(rev)
  rev <- reverseComplement(rev)
  reads <- DNAStringSet(c(as.character(fwd),
                          as.character(rev)))
  names(reads) <- c('fwd', 'rev')
  merged.reads <- merge.reads(reads,minInformation = 0.25, threshold=0.25)
  if(onefile==FALSE){
    nuccode <- list(as.character(merged.reads$consensus))
    names(nuccode) <- samplename
    write.dna(nuccode,
              file=paste0("sangersconsensus/",samplename,".fasta"),
              format="fasta",nbcol=-1,colsep="",colw=1e6L)
  }else{
    nuccode <- list(as.character(merged.reads$consensus))
    names(nuccode) <- samplename
    write.dna(nuccode,
              file="sangersconsensus/consensus.fasta",
              format="fasta",append=TRUE,nbcol=-1,colsep="",colw=1e6L)
  }
  return(list(Sample=samplename,
              Consensuslength=length(merged.reads$consensus),
              Consesusseq=as.character(merged.reads$consensus)))
}
