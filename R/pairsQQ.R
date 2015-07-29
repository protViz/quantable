#' pairsplot of QQ plots
#' @export
#' @param obj dataframe or matrix
#' @param main - title
#' @examples
#'
#' data(SDat)
#' pairsQQ( SDat$Intensity )
#' @seealso \code{\link{qqplot}} and  \code{\link{pairs}}
pairsQQ = function(obj,main=""){
  pairs(obj, panel = function(x,y){
    r <- qqplot(x , y , plot.it = FALSE)
    points(r,pch=".",cex=2)
    abline(0,1,col=2)
  }
  , lower.panel=NULL
  ,main = main
  )
}
