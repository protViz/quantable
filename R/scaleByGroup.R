#' scale data given group 
#' 
#' @param data matrix
#' @param protGroup names matching rownames of data
#' @param plot plot diagnostics
#' @export
#' 
scaleByGroup <- function(data , protGroup, plot=FALSE){
  reference = data[rownames(data) %in% protGroup,]
  noReference = data[!rownames(data) %in% protGroup,]
  referenceScaled = robustscale(reference)
  noReference = sweep(noReference,2,referenceScaled$medians,"-")
  noReference = sweep(noReference,2,referenceScaled$mads,"/")

  if(plot){
    par(mfrow=c(1,2))
    boxplot(noReference,main="noReference",ylim=c(-8,6), pch=".", las=2,cex.axis=0.5)
    abline(h=0,col=2)
    boxplot(referenceScaled$data,main="reference",ylim=c(-8,6),las=2,pch=".",cex.axis=0.5)
    abline(h=0,col=2)
  }
  return(list(reference = referenceScaled$data, noReference = noReference))
}
