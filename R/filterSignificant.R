#' filters significant values and returns them as list of data.frames
#' 
#' @param foldchange log2 fold changes
#' @param pvals p values
#' @param labels e.g. protein ID's
#' @param pthresh pvalue threshold
#' @param foldchangethresh fold change threshold
#' @param biasAdjust adjustment for p-values
#' @examples
#' library(quantable)
#' foldchange <- rnorm(1000)
#' pvals <-rexp(1000)
#' filterSignificant(foldchange, pvals,
#'  rep("blabla",length(pvals)), pthresh=0.1, foldchangethresh=1)
#' filterSignificant(foldchange[foldchange>0], pvals[foldchange>0],
#'  rep("blabla",length(pvals[foldchange>0])), pthresh=0.1, foldchangethresh=1)
#' filterSignificant(foldchange[foldchange<0], pvals[foldchange<0],
#'  rep("blabla",length(pvals[foldchange<0])), pthresh=0.1, foldchangethresh=1)
#' tt <- filterSignificant(foldchange, pvals,
#'  rep("blabla",length(pvals)), pthresh=0.1, foldchangethresh=10)
#' @export
filterSignificant <- function(foldchange,
                        pvals ,
                        labels = NULL,
                        pthresh = 0.05,
                        foldchangethresh = 1,
                        biasAdjust=FALSE
){
  dataframe <- data.frame("foldchange" = foldchange, "pvals" = pvals )
  if(!is.null(labels)){
    dataframe<-data.frame(labels = labels, dataframe) 
  }
  if(biasAdjust){
    medianFC <- median(dataframe$foldchange,na.rm=TRUE)
  }else{
    medianFC <- 0
  }
  
  upsubset<-subset(dataframe,pvals < pthresh & foldchange > medianFC + foldchangethresh)
  downsubset<-subset(dataframe,pvals<pthresh & foldchange < medianFC-foldchangethresh)
  if(nrow(upsubset)>0 & nrow(downsubset)>0){
    return(list(upsubset=data.frame(regulation= "up",upsubset),
                downsubset=data.frame(regulation="down",downsubset)))
  }else if(nrow(upsubset)>0 & nrow(downsubset)==0 ){
    return(list(upsubset=data.frame(regulation= "up",upsubset)))
  }else if(nrow(upsubset)== 0 & nrow(downsubset)> 0 ){
    return(list(downsubset=data.frame(regulation= "up",downsubset)))
  }else{
    res <- data.frame(regulation="NO", dataframe)
    return(res[0,])
  }
}