#' heatmap2 facade
#' @export
#' @param pln or dataframe with numerical values
#' @param distf distance function
#' @param hclustf clustering function
#' @param palette color palette
#' @param main title
#' @param labRow row labels
#' @param ... other parameters to heatmap.2
#' @examples
#' data(SDat)
#' par(mar=c(5,5,5,5))
#' simpleheatmap(SDat$Intensity,ColSideColors=c("red","blue","pink"))
#' simpleheatmap(SDat$Intensity)
simpleheatmap = function(pln,main="",
                         distf=dist,
                         hclustf=hclust,
                         labRow="",
                         palette=div_gradient_pal(low="blue",mid="white",high="red")(seq(0, 1, length = 21)),...)
{
  tmp <- heatmap.2( as.matrix(pln) , trace="none" , scale="none" , col=palette ,
                    labRow=labRow,
                    cexRow=0.1 + 1/log10(dim(pln)[1]),
                    cexCol=0.1 + 1/log10(dim(pln)[2]),
                    distfun=distf,hclustfun=hclustf,
                    margins=c(5,5),main=main,...=...)
}
