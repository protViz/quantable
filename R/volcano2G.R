#' Volcano plot using ggplot and ggrepel
#' @param foldchange vector with fold changes
#' @param pvals vector with pvalues
#' @param labels vector with labels
#' @param pthresh pvalue threshold
#' @param log2FCThresh log2 FC threshold
#' @param main main title
#' @param xlab xlab
#' @param ylab ylab
#' @param xlim xlim
#' @param ylim ylim
#' @param size see geom_text_repel
#' @param segment.size see geom_text_repel
#' @param segement.alpha see geom_text_repel
#' @param pseudo usually q.mod containing NAs
#' @import ggplot2
#' @import ggrepel
#' @examples 
#' rm(list=ls())
#' library(quantable)
#' foldchange <- rnorm(1000)
#' pvals <-rexp(1000)
#' volcano2G(foldchange, pvals,labels=rep("abcde", length(pvals)),
#'   pthresh=0.1, log2FCThresh=0.5,main='test',size=2,segment.size=0.3)
#' 
#' @export
volcano2G <- function(foldchange,
                       pvals,
                       labels,
                       pthresh=0.1,
                       log2FCThresh=0.5,
                       main=NULL,
                       xlab="log2 FC",
                       ylab="-log10(Q Value)",
                       xlim=c(-5,5),
                       ylim=c(0,-log10(min(pvals, na.rm=TRUE))),
                       size=1,
                       segment.size=0.3,
                       segement.alpha=0.3,
                       pseudo = NULL)
{
  results <- data.frame(log2FoldChange = foldchange, pvalue= pvals, labels=labels )
  fcLabel <- paste("Q Value <", pthresh, "& |FC| >", log2FCThresh)
  
  results$significance = ifelse(results$pvalue < pthresh & abs(results$log2FoldChange) > log2FCThresh ,fcLabel ,"Not Sig" )
  if(!is.null(pseudo)){
    results$significance[is.na(pseudo)] <- "pseudo"
  }
  ### hack to pass R CMD check
  log2FoldChange <- NULL
  pvalue <- NULL
  ### 
  p = ggplot(results, aes(log2FoldChange, -log10(pvalue))) +
    geom_point(aes_string(col="significance")) +
    scale_color_manual(values=c("black", "green", "red" ))
  
  p = p + ggplot2::geom_hline(yintercept=-log10(pthresh), col=4, lty=2) 
  p = p + ggplot2::geom_vline(xintercept=c(-log2FCThresh,log2FCThresh), col=4,lty=2) 
  
  filtres <- subset(results, pvalue<pthresh & abs(log2FoldChange)>log2FCThresh )
  p = p + geom_text_repel(data=filtres, aes_string(label='labels'), size=size, segment.size = segment.size, segment.alpha = segement.alpha)
  if(!is.null(main)){
    p = p + ggtitle(main)
  }
  p = p + xlab(xlab)
  p = p + ylab(ylab)
  p = p + xlim(xlim[1],xlim[2])
  p = p + ylim(ylim[1],ylim[2])
  
  return(p)
}

