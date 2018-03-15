#' A dataframe called "outreps" with sequence identifiers and aligned sequences
#'
#' A dataset containing representative sequences from a MiSeq V4 run at the end
#' of the default mothur pipeline at CMET (alignment to the mothur recreated
#' SILVA seed release 123, and filtered). Typically, the sequences are chosen
#' based upon the most abundant sequence within an individual OTU.
#'
#' @format A data frame with 21640 rows and 2 variables:
#' \describe{
#'   \item{OTU}{sequence identifier Otu00001-Otu21640}
#'   \item{readseq}{filtered aligned DNA sequence}
#' }
"otureps"
