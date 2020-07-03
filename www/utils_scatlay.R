######## FOR NORMALIZATION ##################
############################################
RUVg.apply <- function(raw_counts, spikes){
  # f = list of types
  
  set <- newSeqExpressionSet(as.matrix(raw_counts))
  # upper-quartile normalization by EDASeq
  set <- betweenLaneNormalization(set, which="upper")
  if(! is.null(spikes) ) {
    spikes <- intersect(spikes,rownames(raw_counts))
    set1 <- RUVg(set, spikes, k=1) # spikes = negative control genes
    return(set1)
  } else {
    return(set)
  }
}

getEmpirical <- function(raw_counts,f){
  # filter data by at least 1 value with >0 counts for each gene
  filter <- apply(raw_counts, 1, function(x) length(x[x>0])>=2)
  filtered <- raw_counts[filter,]
  set <- newSeqExpressionSet(as.matrix(filtered), phenoData = data.frame(f, row.names=colnames(raw_counts)))
  
  design <- model.matrix(~f, data=pData(set))
  y <- DGEList(counts=counts(set), group=f)
  y <- calcNormFactors(y, method="upperquartile")
  y <- estimateGLMCommonDisp(y, design)
  y <- estimateGLMTagwiseDisp(y, design)
  fit <- glmFit(y, design)
  lrt <- glmLRT(fit, coef=2)
  top <- topTags(lrt, n=nrow(set))$table
  n_row <- nrow(filtered)
  empirical <- rownames(set)[which(!(rownames(set) %in% rownames(top)[1:round(0.2*n_row)]))]
  # set2 <- RUVg(set, empirical, k=1)
  return(empirical)
}

tpm<- function(counts, lengths){
  rate <- counts/lengths
  tpm <- rate/sum(rate) * 1e6
  return (tpm)
}

fpkm <- function(counts,lengths){
  rate <- counts/lengths
  fpkm <- rate/sum(counts) * 1e9
}

############ LOAD PACKAGES #################
############################################

loadPkg <- function() {
  
  if(length(find.package(package = 'shinythemes',quiet = T))>0){
    library(shinythemes)
  }else{
    print("Package shinythemes not installed")
    install.packages("shinythemes")
    print("Package shinythemes installed")
    library(shinythemes)
  }
  
  if(length(find.package(package='tidyverse',quiet=T)) == 0){
    install.packages("tidyverse")
  }
  library(tidyverse)
  
  if(length(find.package(package = 'ggplot2',quiet = T))>0){
    library(ggplot2)
  }else{
    print("Package ggplot2 not installed")
    install.packages("ggplot2")
    print("Package ggplot2 installed")
    library(ggplot2)
  }
  
  if(length(find.package(package='BiocManager',quiet=T)) == 0){
    install.packages("BiocManager")
  }

  if(length(find.package(package='RUVSeq',quiet=T)) == 0){
    BiocManager::install("RUVSeq", update=FALSE)
  }
  library(RUVSeq)
  
  
  if(length(find.package(package = 'dplyr',quiet=T))>0){
    library(dplyr)
  }else{
    print("Package dplyr not installed")
    install.packages("dplyr")
    print("Package dplyr installed")
    library(dplyr)
  }
  
  
  if(length(find.package(package = 'DT',quiet = T))>0){
    library(DT)
  }else{
    print("Package DT not installed")
    install.packages("DT")
    print("Package DT installed")
    library(DT)
  }

  
  if(length(find.package(package = 'plotly',quiet = T))>0){
    library(plotly)
  }else{
    print("Package plotly not installed")
    install.packages("plotly")
    print("Package plotly installed")
    library(plotly)
  }
  
  
  if(length(find.package(package = 'ks',quiet = T))>0){
    library(ks)
  }else{
    install.packages("ks")
    library(ks)
  }

}
