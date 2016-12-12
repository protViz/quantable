
#' Volcano plot using ggplot and ggrepel
#' @param foldchange vector with fold changes
#' @param pvals vector with pvalues
#' @param labels vector with labels
#' @param pthresh pvalue threshold
#' @param log2FCThresh log2 FC threshold
#' @param main main title
#' @import ggplot2
#' @import ggrepel
#' @examples 
#' library(quantable)
#' foldchange <- rnorm(1000)
#' pvals <-rexp(1000)
#' volcano2G(foldchange, pvals,labels=rep("abcde", length(pvals)),
#'   pthresh=0.1, log2FCThresh=0.5,main='test')
#' 
#' @export
volcano2G <- function(foldchange, pvals, labels, pthresh=0.1, log2FCThresh=0.5, main=NULL){
  
  results <- data.frame(log2FoldChange = foldchange, pvalue= pvals, labels=labels )
  fcLabel <- paste("p <", pthresh, "& |FC| >", log2FCThresh)
  
  results$sig = ifelse(results$pvalue<pthresh & abs(results$log2FoldChange) > log2FCThresh ,fcLabel , "Not Sig")
  ### hack to pass R CMD check
  log2FoldChange <- NULL
  pvalue <- NULL
  ### 
  p = ggplot(results, aes(log2FoldChange, -log10(pvalue))) +
    geom_point(aes_string(col="sig")) +
    scale_color_manual(values=c("black", "red"))
  filtres <- subset(results, pvalue<pthresh & abs(log2FoldChange)>log2FCThresh )
  p = p + geom_text_repel(data=filtres, aes_string (label='labels'))
  if(!is.null(main)){
    p = p + ggtitle(main)
  }
  return(p)
}
