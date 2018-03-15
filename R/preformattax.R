#' Reformatter of taxonomy files from mothur for use in R
#'
#' This function uses a taxonomy file (i.e. an RDP naive bayesian fixed-rank
#' asignment detail) and pre-processes it into a data frame for use within R.
#'
#' @param taxonomy is a taxonomy file, raw.
#' @param OTUtax is a logical indicating wether we are dealing with a
#' classify.otu (TRUE) or classify.seqs (FALSE) taxonomy file. Defaults to
#' TRUE.
#' @param probab is a logical indicating whether the classify command was run
#' with probs =T (i.e. probabilities or bootstrap confidences are included).
#' Defaults to TRUE (= probs are in the taxonomy file). Currently ignored for
#' if FALSE.
#' @param keepProbab is a logical indicating whether or not to keep the columns
#' with the probabilities. Defaults to TRUE. Ignored when probab is FALSE
#' @param keepOTU is a logical indicating whether or not to include a column with
#' the OTU names called OTU. Defaults to FALSE.
#' @param keepSize is a logical indicating whether or not to include a column
#' that contains the total read count within the OTU. Defaults to FALSE.
#' @importFrom splitstackshape cSplit
#' @importFrom dplyr contains
#' @keywords converter
#' @return A dataframe with a splitted taxonomy file.
#' The Size column is dropped.
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find testset
#'
#' @export

preformattax <- function(taxonomy, OTUtax=TRUE, probab=TRUE, keepProbab=TRUE,
                         keepOTU=FALSE, keepSize=FALSE)
{

  tax.good <- splitstackshape::cSplit(taxonomy,"Taxonomy",";")
  if(OTUtax==TRUE){
    tax.good.probs <- do.call(cbind,
                              lapply(tax.good[,3:ncol(tax.good)], function(x){
                                       do.call(rbind,
                                               strsplit(as.character(x),
                                                        "\\((?=\\d)",
                                                        perl = TRUE))
                                }))
  }else{
    tax.good.probs <- do.call(cbind,
                              lapply(tax.good[,2:ncol(tax.good)], function(x){
                                do.call(rbind,
                                        strsplit(as.character(x),
                                                 "\\((?=\\d)",
                                                 perl = TRUE))
                              }))
  }
  tax.final <- as.data.frame(apply(tax.good.probs,2,
                                   function(x) sub(")","",x)))
  if(OTUtax==TRUE){
    otunames <- tax.good$OTU
    rownames(tax.final) <- otunames
  }else{
    seqnames <- tax.good[,1]
    rownames(tax.final) <- seqnames
  }
  colnames(tax.final) <- c("Regnum","Prob_R","Phylum","Prob_P",
                           "Classis","Prob_C","Ordo","Prob_O",
                           "Familia","Prob_F","Genus","Prob_G")
  if(ncol(tax.final)==12){
    #for RDP & SILVA taxonomies which stop at genus level
    tax.final <- as.data.frame(dplyr::mutate(tax.final,Species=NA,Prob_S=NA))
    rownames(tax.final) <- otunames
  }else{
    #for greengenes taxonomy
    colnames(tax.final)[13:14] <- c("Species","Prob_S")
    tax.final <- as.data.frame(apply(tax.final,
                                     2,
                                     function(x)sub("[a-z]__","",x)))
    tax.final$Species <- paste(tax.final$Genus,tax.final$Species) #just the species is not very sensible
  }
  #to comply to mothur 1.38 tax format
  charvectgenus <- as.character(tax.final$Genus)
  probvectgenus <- as.numeric(as.character(tax.final$Prob_G))
  nasgenuslev <- which(is.na(charvectgenus))
  for(i in nasgenuslev)
  {
    if(grepl("unclassified",as.character(tax.final$Familia[i])))
    {
      if(!is.na(as.character(tax.final$Familia[i])))
      {
        charvectgenus[i]<-as.character(tax.final$Familia[i])
      }else{
        if(grepl("unclassified",as.character(tax.final$Ordo[i])))
        {
          charvectgenus[i]<-as.character(tax.final$Ordo[i])
        }
      }

    }else{
      if(!is.na(as.character(tax.final$Familia[i])))
      {
        charvectgenus[i]<-paste0(as.character(tax.final$Familia[i]),"_unclassified")
      }else{
        charvectgenus[i]<-paste0(as.character(tax.final$Ordo[i]),"_unclassified")
      }

    }
    probvectgenus[i] <- as.numeric(as.character(tax.final$Prob_F[i]))
  }
  tax.final$Genus <- factor(charvectgenus)
  tax.final$Prob_G <- probvectgenus

  charvectfamilia <- as.character(tax.final$Familia)
  probvectfamilia <- as.numeric(as.character(tax.final$Prob_F))
  nasfamlev <- which(is.na(charvectfamilia))
  for(i in nasfamlev)
  {
    if(grepl("unclassified",as.character(tax.final$Ordo[i])))
    {
      if(!is.na(as.character(tax.final$Ordo[i])))
      {
        charvectfamilia[i]<-as.character(tax.final$Ordo[i])
      }else{
        if(grepl("unclassified",as.character(tax.final$Classis[i])))
        {
          charvectfamilia[i]<-as.character(tax.final$Classis[i])
        }
      }

    }else{
      if(!is.na(as.character(tax.final$Ordo[i])))
      {
        charvectfamilia[i]<-paste0(as.character(tax.final$Ordo[i]),
                                   "_unclassified")
      }else{
        charvectfamilia[i]<-paste0(as.character(tax.final$Classis[i]),
                                   "_unclassified")
      }

    }
    probvectfamilia[i] <- as.numeric(as.character(tax.final$Prob_O[i]))
  }
  tax.final$Familia <- factor(charvectfamilia)
  tax.final$Prob_F <- probvectfamilia
  #TODO: implement if NA's occur heigher than family level
  if(keepProbab==TRUE){
    tax.final <- tax.final
  }else{
    tax.final <- tax.final %>% dplyr::select(-contains("Prob"))
  }
  if(keepOTU==TRUE){
    tax.final <- data.frame(OTU=rownames(tax.final),tax.final)
  }
  if(keepSize==TRUE){
    tax.final <- data.frame(Size=tax.good$Size,tax.final)
  }
  return(tax.final)
}
