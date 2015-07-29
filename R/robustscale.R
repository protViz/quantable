#' robust scaling
#' uses median an mad instead of mean and row
#' applies the scaling to the columns (samples) by default
#' @export
#' @param data matrix or data.frame
#' @param dim - should rows (1) or columns (2:default) be scaled
#' @param center - subract median (default:TRUE)
#' @param scale - scale by mad  (default:FALSE)
#' @examples
#' data(SDat)
#' boxplot(SDat$Intensity)
#' tmp = asinh(SDat$Intensity)
#' tmp = robustscale(tmp)
#' boxplot(tmp$data)
robustscale <- function(data, dim=2, center=TRUE, scale=TRUE){
  medians = NULL
  if(center){
    medians <- apply(data,dim,median,na.rm=TRUE)
    data = sweep(data,dim,medians,"-")
  }
  mads=NULL
  if(scale){
    mads <- apply(data,dim, mad,na.rm =TRUE)
    data = (sweep(data,dim,mads,"/"))
  }
  return(list(data=data,medians=medians,mads=mads))
}
