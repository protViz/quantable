#' volcano plot
#' @param foldchange - fold change values
#' @param pvals pvalues
#' @param pthresh pvalue threshold
#' @param foldchangethresh threshold of foldchange
#' @param labels - optional labels
#' @param cex size of labels
#' @param xlab - x axis label
#' @param ylab - y axis label
#' @param xlim - xlim
#' @param cex.point - point size
#' @param main - main title
#' @param biasAdjust - if bias in foldchanges exists (i.e. if median of fold changes does not equal 0) you can use this option to adjust for it.
#' 
#' @export
#' @examples
#' foldchange <- rnorm(1000)
#' pval <-rexp(1000)
#' abline(v=0.05,col=2)
#'
#' volcanoplot(foldchange, pval,pthresh=0.1, foldchangethresh=1,main='test')
#'
volcanoplot = function(foldchange,
                       pvals ,
                       pthresh = 0.05,
                       foldchangethresh = 1,
                       xlab ="log2(T/N)" ,
                       ylab = "-log10(P)",
                       labels = NULL,
                       cex=0.6,
                       cex.point=1,
                       xlim=NULL,
                       main=NULL, biasAdjust=FALSE
){
  dataframe <- data.frame("foldchange" = foldchange, "pvals" = pvals )
  
  if(!is.null(labels)){
    dataframe<-data.frame(labels = labels, dataframe) 
  }
  
  bla = tryCatch( plot(dataframe$foldchange,-log10(dataframe$pvals),col="#00000033",pch=19,xlab=xlab, ylab=ylab,xlim=xlim,main=main),
                  warning=function(bla){ dev.off(); return(1)}
  )
  if(!is.null(bla)){
    plot(dataframe$foldchange,-log10(dataframe$pvals),col=1,pch=19,xlab=xlab, ylab=ylab ,xlim=xlim,cex=cex.point,main=main)
  }
  if(biasAdjust){
    medianFC <- median(dataframe$foldchange,na.rm=TRUE)
  }else{
    medianFC <- 0
  }

  upsubset<-subset(dataframe,pvals < pthresh & foldchange > medianFC + foldchangethresh)
  points(upsubset$foldchange,-log10(upsubset$pvals),col=2,pch=19)
  points(upsubset$foldchange,-log10(upsubset$pvals),col=1,pch=1)
  if(length(rownames(upsubset)) > 0){
    text(upsubset$foldchange, -log10(upsubset$pvals),rownames(upsubset),cex=cex,pos=2)
  }

  abline(h=-log10(pthresh),col="gray")
  downsubset<-subset(dataframe,pvals<pthresh & foldchange < medianFC-foldchangethresh)
  points(downsubset$foldchange,-log10(downsubset$pvals),col=3,pch=19)
  points(downsubset$foldchange,-log10(downsubset$pvals),col=1,pch=1)
  if(length(rownames(downsubset)) > 0){
    text(downsubset$foldchange, -log10(downsubset$pvals),rownames(downsubset),cex=cex,pos=4)
  }

  abline(v=c(medianFC-foldchangethresh,medianFC+foldchangethresh),lty=2)
  abline(v =medianFC,lty=2,lwd=1.5)
  return(list(upsubset=data.frame(regulation= "up",upsubset),
              downsubset=data.frame(regulation="down",downsubset)))
}
