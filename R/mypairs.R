#' normal pairs plot with different pch and plus abline
#' @param dataframe data matrix or data.frame as normally passed to pairs
#' @param ... params usually passed to pairs
#' @export
#' @examples
#' data(SDat)
#' mypairs(SDat$Intensity,log="xy",main="small data")
#' @seealso also \code{\link{pairs}}
mypairs = function(dataframe,...){
  pairs(dataframe, panel = function(x,y){
    points(x, y, pch=".")
    abline(a=0,b=1,v=0,h=0,col=2)
  }
  , lower.panel=NULL,...
  )
}
