.transitionCorrelations <- function(dataX , method="spearman"){
  if(nrow(dataX) > 1){
    ordt <- (dataX)[order(apply(dataX,1,mean)),]
    dd <- stats::cor(t(ordt),use="pairwise.complete.obs", method = method)
    dd[is.na(dd)] <- -1
    return(dd)
  }else{
    message("Could not compute correlation, nr rows : " , nrow(dataX) )
  }
}

.findDecorrelated <- function(res, threshold = 0.7){
  nrtrans <- ncol(res)
  ids <- rowSums(res < threshold, na.rm = TRUE)
  names(which((nrtrans-1)== ids))
}

#' remove decorrelated rows
#' @param ff matrix or data frame
#' @param corThreshold correlation threshold
#' @param tr intensity transformation to apply
#' @export
removeDecorrelated <- function(ff, corThreshold = 0.7, tr = identity ){
  fx <-tr(ff)
  res <-.transitionCorrelations(fx, method="pearson")
  ff[!ff$EG.PrecursorId%in% .findDecorrelated(res,threshold = corThreshold), ]
}


plotNicely <- function(dataX, main="", log="", ylab="log(intensity)"){
  mat <- matrix(c(1,1,1,0,2,3), byrow=T, ncol=3)
  graphics::layout(mat, widths=c(5,2,2), heights=c(2,1))
  dataXt <- t(dataX)
  graphics::matplot(dataXt,type="b", main=main,lwd=1,lty=1, ylab="log(intensity)",las=2, xaxt = "n", log=log)
  graphics::axis(1, at = 1:nrow(dataXt), labels = rownames(dataXt), cex.axis = 0.7, las=2)
  graphics::legend("bottomleft", legend=rownames(dataX),col=1:5,lty=1 )
  nrow(dataX)
  if(nrow(dataX)>1){
    ordt <- (dataX)[order(apply(dataX,1,mean)),]
    dd <- stats::cor(t(ordt),use="pairwise.complete.obs", method = "spearman")
    imageWithLabelsNoLayout(dd,col=getBlueWhiteRed(),zlim=c(-1,1))
    imageColorscale(dd,col=getBlueWhiteRed(), zlim=c(-1,1))
    invisible(dd)
  }
  
}

