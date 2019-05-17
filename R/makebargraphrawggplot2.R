#' ggplot2-based cummulative stacked bargraphs for plotting taxonomy
#'
#' This function uses a taxonomy file (i.e. an RDP naive bayesian fixed-rank
#' asignment detail) and a shared file to create 100% summed stacked bargraphs.
#' Only the top-n OTUs (in relative abundance) among all samples are displayed.
#' All other OTUs are summed into a part of the graph designated as "other".
#' The resulting object contains information about the composition of "other".
#'
#' @param tax is a taxonomy file, raw.
#' @param shared is a shared (OTU-by-sample) file, filtered per sample group.
#' @param topn is the top number of taxa selected to display (maximum 11,
#'  default 8)
#' @param shared.abs is a logical showing wether the shared file already
#'  consists out of direct Illumina counts, or is really the raw csv from
#'  Ramiro (default TRUE)
#' @param taxlevel is either one of  "Regnum", "Phylum", "Classis",
#'  "Ordo",  "Familia", "Genus"
#' @param tax.prob shows whether the taxonomy file contains probabilites
#'  (default FALSE)
#' @param tresh is a threshold for the probability taxonomy duplicate matching
#'  (to be implemented, default 85)
#' @param samples is an optional string for the title of the generated bargraph
#'  (e.g. "MOB","OLAND","Fecal samples", defualt empty)
#' @param viridfill whether or not a discrete viridis fill pallette should be
#'  used (defaults to FALSE)
#' @param plot states wether or not to print the plot within the function
#'  (default TRUE)
#' @param ... optional arguments passed on to ggplot call
#' @importFrom dplyr desc funs
#' @importFrom vegan decostand
#' @importFrom ggplot2 ggplot aes geom_bar theme element_text labs theme_minimal scale_fill_brewer
#' @importFrom reshape2 melt
#' @importFrom viridis scale_fill_viridis
#' @keywords taxonomic diversity
#' @return a list of 3 objects: ggplotdf (the underlying data for the ggplot),
#'  othertax (an abundance-sorted dataframe of the OTUs summarized in "other"),
#'  ggplotobj (the generated ggplot2 object)
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find testset
#'
#' @export

makebargraphrawggplot2<-function(tax,shared,topn=8,
                                 taxlevel=c("Regnum","Phylum", "Classis", "Ordo",
                                            "Familia", "Genus","Species"),
                                 shared.abs=T,tax.prob=F,tresh=85,samples,
                                 viridfill=FALSE,plot=TRUE,...)
{
  stopifnot(topn<=11)
  if(topn<=8){brewset<-"Set1"}else{brewset<-"Set3"}
  ####match selected taxlevel####
  taxlevel<-match.arg(taxlevel)

  ####make sure shared file hase absolute counts to start with####
  if(shared.abs==T)
  {
    shared=shared
  }else{
    totseqpersamp<-shared[nrow(shared),]
    shared<-shared[-nrow(shared),]
    effectivereads<-totseqpersamp*colSums(shared)/100
    shared<-sweep(shared,MARGIN=2,as.numeric(effectivereads),'*')
    #from the Ramiro-Era
  }

  seltax.str<-paste(as.character(bquote(tax)),taxlevel,sep="$")
  seltax.val<-eval(parse(text=seltax.str))
  unclass.val<-as.matrix(seltax.val) #variable with highest classifiable taxon
  # of unclassifieds
  # raw shared file, tax not preprocessed yet, auto-assign unclassified to
  # closest taxlevel
  unclass.index <- grep("unclassified",seltax.val)
  incert.index  <- grep("Incertae",seltax.val)
  #index of all "unclassified" and incertae sedis at the current taxonomic level
  if(length(unclass.index)!=0)
  {
    for(i in 1:length(unclass.index))
    {
      unclass.rowidx<-unclass.index[i] #select unclassified row per row
      class.colidx<-min(grep("unclassified",
                             as.character(as.matrix(tax[unclass.rowidx,]))))-1
      #column index of deepest assigned taxon
      #consitency check to avoid stuff like Gp4 etc
      untax <- as.matrix(tax)[unclass.index[i],class.colidx]
      if(grepl("[0-9]",untax)&&!(taxlevel%in%c("Kingdom","Phylum"))){
        while(grepl("[0-9]",untax)&&class.colidx>=1){
          #browser()
          class.colidx <- class.colidx-1
          untax <- as.matrix(tax)[unclass.index[i],class.colidx]
        }
      }
      #maybe consider other threshold so that for summing e.g. phyla is allowed
      if(class.colidx>=1)
      {
        unclass.val[unclass.index[i]]<-paste("unclassified ",
                                             as.matrix(tax)[unclass.index[i],
                                                            class.colidx],
                                             sep="")
      }else{unclass.val[unclass.index[i]]<-"unclassifiable"}
    }
  }
  if(length(incert.index)!=0)
  {
    for(i in 1:length(incert.index))
    {
      incert.rowidx<-incert.index[i] #select unclassified row per row
      class.colidx<-min(grep("Incertae",
                             as.character(as.matrix(tax[incert.rowidx,]))))-1
      #column index of deepest assigned taxon
      if(class.colidx>=1) #maybe consider other threshold so that for summing e.g. phyla are allowed
      {
        unclass.val[incert.index[i]]<-paste("unclassified ",
                                            as.matrix(tax)[incert.index[i],
                                                           class.colidx],sep="")
      }else{unclass.val[incert.index[i]]<-"unclassifiable"}
    }
  }
  unclass.val<-as.factor(unclass.val)

  #### Sum seqcount over all duplicate taxa ####
  if(tax.prob==F)
  {
    #first assign different unclassified to all unclassified OTU's at selected level
    seltax.val<-as.matrix(seltax.val)
    seltax.val[grep("unclassified",seltax.val)] <- paste(rep("unclassified",
                                    length(grep("unclassified",seltax.val))),
                                    seq(1,length(grep("unclassified",
                                                      seltax.val))),sep="")
    seltax.val[grep("Incertae",seltax.val)] <- paste(rep("Incertae",
                                    length(grep("Incertae",seltax.val))),
                                    seq(1,length(grep("Incertae",
                                                      seltax.val))),sep="")
    seltax.val<-as.factor(seltax.val)
    #df that matches unclass with deepest possible classification
    compclass.val<-data.frame(current=seltax.val,classified=unclass.val)
    #then find duplicate values
    multtax<-levels(as.factor(as.character(seltax.val[duplicated(seltax.val)])))
    #then take the sum of the duplicate values
    shared.tax<-data.frame(shared,seltax.val)
    shared.mfsum<-matrix(0,nrow=length(multtax),ncol=ncol(shared))
    colnames(shared.mfsum)<-names(shared)
    #colsummed matrix for duplicate entries
    for(i in 1:length(multtax))
    {
      shared.mfsum[i,] <- colSums(shared.tax[which(shared.tax$seltax.val==multtax[i]),
                                             1:ncol(shared.tax)-1])
    }
    shared.mfsum.tax<-data.frame(shared.mfsum,multtax)
    #contains all summed sequences of all duplicated or more assigned taxa
    shared.tax.single <- shared.tax[shared.tax$seltax.val %in%
                                      setdiff(shared.tax$seltax.val,
                                              shared.mfsum.tax$multtax),]
    #select all single occurences
    names(shared.mfsum.tax) <- names(shared.tax.single)
    #make sure that names are the same for rbind command
    shared.tax.final<-rbind(shared.mfsum.tax,shared.tax.single)
    rownames(shared.tax.final) <- shared.tax.final$seltax.val
    shared.tax.final <- subset(shared.tax.final,select=-seltax.val)
  }else
  {
    #first assign different unclassified to all unclassified OTU's at selected level
    seltax.val<-as.matrix(seltax.val)
    seltax.val[grep("unclassified",seltax.val)] <- paste(rep("unclassified",
                                  length(grep("unclassified",seltax.val))),
                                  seq(1,length(grep("unclassified",seltax.val))),
                                  sep="")
    seltax.val<-as.factor(seltax.val)
    #df that matches unclass with deepest possible classification
    compclass.val<-data.frame(current=seltax.val,classified=unclass.val)
    #then find duplicate values -- here's the difficulty when using probabilities
    # the next command clearly illustrates the issue:
    # levels(as.factor(as.character(seltax.val[duplicated(seltax.val)])))
    # use the data without the probabilities for the summing
    # sub("\\(.*\\)","",seltax.val)
    # new proposal: everything above a certain bootstrap value gets summed
    # (10000 Wang iterations)

    # make df to do thresholding upon
    selframe <- data.frame(do.call(rbind,
                                   strsplit(sub("\\(","\\.\\(",
                                      as.character(levels(seltax.val))),"\\.")))
    #as.character(levels(seltax.val))
    selframe <- cbind(selframe,as.character(levels(unclass.val)))
    names(selframe) <- c("taxon","bootstrap","full")

    selframe$bootstrap <- as.numeric(sub("\\((.*)\\)","\\1",selframe$bootstrap))
    selframe.duplo <- selframe[duplicated(selframe$taxon)|
                                 duplicated(selframe$taxon,fromLast=T),]
    #zeer elegante oplossing!
    selframe.duplo.select <- selframe.duplo[selframe.duplo$bootstrap>tresh,]
    treshedmulttax <- levels(as.factor(as.character(
      selframe.duplo.select[duplicated(selframe.duplo.select$taxon),]$taxon)))
    shared.treshed.sum <- matrix(0,nrow=length(treshedmulttax),ncol=ncol(shared))
    colnames(shared.treshed.sum)<-names(shared)
    rownames(shared.treshed.sum)<-treshedmulttax
    shared.tax<-data.frame(shared,seltax.val)
    for(i in 1:length(treshedmulttax))
    {
      corresp<-dupframe.select[dupframe.select$taxon==treshedmulttax[i],"full"]
      #print(corresp)
      #print(which(as.character(shared.tax$seltax.val)==as.character(corresp)))
      for(j in 1:length(corresp))
      {
        shared.treshed.sum[i,]<-shared.treshed.sum[i,] +
          colSums(shared.tax[which(as.character(shared.tax$seltax.val)==
                                     as.character(corresp[j])),
                             1:ncol(shared.tax)-1])
      }
    }
    shared.treshed.sum<-data.frame(shared.treshed.sum,rownames(shared.treshed.sum))
    shared.tax.single<-shared.tax[shared.tax$seltax.val%in%setdiff(shared.tax$seltax.val,levels(selframe.duplo.select$full)),] #select all single occurences
    names(shared.treshed.sum)<-names(shared.tax.single)
    #make sure that names are the same for rbind command
    shared.tax.final<-rbind(shared.treshed.sum,shared.tax.single)
    rownames(shared.tax.final)<-shared.tax.final$seltax.val
    shared.tax.final<-subset(shared.tax.final,select=-seltax.val)

  }


  ####Order shared file####
  OTU.count.tax<-rowSums(shared.tax.final) #total count of each assigned OTU
  data_count_tax<-data.frame(shared.tax.final,OTU.count.tax)
  data_tax <-data_count_tax[order(-data_count_tax$OTU.count.tax),] #sort descending
  data_matrix_tax_final<-data.matrix(data_tax)

  #select top  n OTUs, and remove OTU.count column
  dimord<-dim(data_matrix_tax_final)
  data_matrix_tax_top <- data_matrix_tax_final[1:topn,1:(dimord[2]-1)]
  if(topn>dimord[1]){
    stop(paste("You want to select a number of taxa (topn=",topn,
               ") which exceeds the amount of taxa (",dimord[1],
               ") at the selected taxonomy level (",taxlevel,
               "). Please adjust one of them"))
  }
  if(topn==dimord[1]){
    warning(paste("You selected a number of taxa(topn=",topn,
                  ") which exactly equals the number of taxa",
                  "at the selected taxonomy level.",
                  "No other taxon group will be shown."))
  }
  if((topn+1)<=dimord[1]){
  data_matrix_tax_other <- data_matrix_tax_final[(topn+1):dimord[1],1:(dimord[2]-1)]
  othertax <- data_matrix_tax_other
  } else {
    othertax <- NULL
  } # this situation can happen e.g. at phylum level
  if(!is.null(dim(othertax)))
  {
    rownames(othertax)<- plyr::mapvalues(rownames(data_matrix_tax_other),
                                         from=as.character(compclass.val$current),
                                         to=as.character(compclass.val$classified),warn_missing = FALSE)
    taxum <- rowSums(othertax)
    #df containing all other taxa, ranked
    suppressWarnings(othertax <- data.frame(othertax,classif=rownames(othertax),taxsum=taxum))
    othertax <- othertax %>% dplyr::group_by(classif) %>%
      dplyr::summarise_all(list(sum)) %>%
      dplyr::arrange(desc(taxsum))
    #format data for plotting
    data_matrix_tax_plot<-rbind(data_matrix_tax_top,colSums(data_matrix_tax_other))
    #take a sum over all the 'other' sequences
    data_matrix_tax_stand_plot<-decostand(data_matrix_tax_plot,method="total",MARGIN=2)
    #relatieve counts per staal
  }else{
    data_matrix_tax_plot <- data_matrix_tax_top
    data_matrix_tax_stand_plot<-decostand(data_matrix_tax_plot,method="total",MARGIN=2)
  }
  #normally, at this point, we would assign names to the "unclassifieds"
  if(tax.prob==F)
  {
    unclass.plot.idx<-grep("unclassified",rownames(data_matrix_tax_stand_plot))
    #indices of not assigned taxa for the plot
    incert.plot.idx <- grep("Incertae",rownames(data_matrix_tax_stand_plot))
    if(length(unclass.plot.idx)!=0){
      for(i in 1:length(unclass.plot.idx))
      {
        rownames(data_matrix_tax_stand_plot)[unclass.plot.idx[i]] <-
          as.character(compclass.val[compclass.val$current==rownames(data_matrix_tax_stand_plot)[unclass.plot.idx[i]],]$classified)
      }}
    if(length(incert.plot.idx)!=0){
      for(i in 1:length(incert.plot.idx))
      {
        rownames(data_matrix_tax_stand_plot)[incert.plot.idx[i]] <-
          as.character(compclass.val[compclass.val$current==rownames(data_matrix_tax_stand_plot)[incert.plot.idx[i]],]$classified)
      }}
    #check for replicated unclassifieds (!) ==> see if summing is not more "sensible" after all?
    if(length(unique(rownames(data_matrix_tax_stand_plot)[duplicated(rownames(data_matrix_tax_stand_plot))]))!=0){
      dupnames.sto <- unique(rownames(data_matrix_tax_stand_plot)[duplicated(rownames(data_matrix_tax_stand_plot))])
      for(i in dupnames.sto)
      {
        whichdupnames <- grep(i,
                              rownames(data_matrix_tax_stand_plot))
        rownames(data_matrix_tax_stand_plot)[whichdupnames]<-paste0(i,1:length(whichdupnames))
      }

    }
  }
  if((topn+1)<=dimord[1]){
  rownames(data_matrix_tax_stand_plot)[nrow(data_matrix_tax_stand_plot)]<-"other"
  }
  ####actual plotting####
  datamatmelt<-melt(data_matrix_tax_stand_plot)

  datamatmelt$Var2o<-factor(datamatmelt$Var2,unique(as.character(datamatmelt$Var2))) #keep original order
  p<-ggplot(data=datamatmelt,aes(x=factor(Var2o),y=value), ...)+
    geom_bar(stat="identity",aes(fill=factor(Var1,levels=rev(levels(Var1)))))+
    theme_minimal(base_size=10) + theme(axis.text.x=element_text(angle=90,
                                                                 hjust=1))+
    labs(x=NULL,y="Relative abundance",
         fill=paste("Deepest taxon \n(",taxlevel," level)",sep=""),
         title=paste("Taxonomic distribution ",samples," samples",sep=""))

  if(viridfill==TRUE){
    p <- p + scale_fill_viridis(option = "D", discrete=TRUE)
  } else {
    p <- p + scale_fill_brewer(palette=brewset,direction = -1)
  }

  if(plot==TRUE){print(p)}

  reslist <- list(ggplotdf=datamatmelt,othertax=othertax,ggplotobj=p)
  return(reslist)
}
