#' Conversion of data frames with correct layout to fasta files.
#'
#' Typically used in conjunction with fasta2dataframe.
#' @param dataframe a dataframe containing sequence identifiers and sequences
#'  in separate columns. with column names
#' @param seqIDcolumn string indicating the name of the column with the
#'  sequence identifiers (defaults to SeqIDs).
#' @param seqColumn string indicating the name of the column with the
#'  sequences themselves (defaults to readseq)
#' @param filename desirec filename (defaults to fastadf)
#' @importFrom Biostrings DNAStringSet writeXStringSet
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: add export option
#'
#' @export

 df2fasta <- function(dataframe,filename="fastadf",seqIDcolumn="SeqIDs",
                      seqColumn="readseq"){
   if(sum(colnames(dataframe)==seqIDcolumn)!=1){
     stop(paste("Your dataframe doesnot contain a single column named",
                seqIDcolumn, ", please correct"))
   }
   if(sum(colnames(dataframe)==seqColumn)!=1){
     stop(paste("Your dataframe doesnot contain a single column named",
                seqColumn, ", please correct"))
   }

   identifiers <- dataframe %>% dplyr::select(eval(seqIDcolumn))
   seq <- dataframe %>% dplyr::select(eval(seqColumn))
   dna <- Biostrings::DNAStringSet(seq)
   names(dna) <- identifiers
   Biostrings::writeXStringSet(dna, paste(filename,".fasta",sep=""))
 }
