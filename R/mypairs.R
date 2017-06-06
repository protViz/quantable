#' normal pairs plot with different pch and plus abline
#' @param dataframe data matrix or data.frame as normally passed to pairs
#' @param ... params usually passed to pairs
#' @param legend  add legend to plots
#' @param pch point type default "."
#' @export
#' @examples
#' tmp = matrix(rep((1:100),times = 4) + rnorm(100*4,0,3),ncol=4)
#' mypairs(tmp,log="xy",main="small data")
#' mypairs(tmp,log="xy",main="small data", legend=TRUE)
#' @seealso also \code{\link{pairs}}
mypairs = function(dataframe,legend=FALSE,pch=".",...){
  pairs(dataframe, panel = function(x,y){
    graphics::points(x, y, pch=pch)
    graphics::abline(a=0,b=1,v=0,h=0,col=2)
    if(legend){
      cR2 <- stats::cor(x,y, use="pairwise.complete.obs")^2
      graphics::legend("topleft", legend=paste("R^2=", round(cR2, digits=2) , sep=""),text.col=3)}
    }
    , lower.panel=NULL,...
  )
}

#' smoothScatter pairs
#' @param dataframe data matrix or data.frame as normally passed to pairs
#' @param ... params usually passed to pairs
#' @param legend  add legend to plots
#' @param pch point type default "."
#' @export
#' @examples
#' tmp = matrix(rep((1:100),times = 4) + rnorm(100*4,0,3),ncol=4)
#' mypairsSmooth(tmp,main="small data", legend=TRUE)
#' mypairsSmooth(tmp,log="xy",main="small data")
#' mypairsSmooth(tmp,log="xy",main="small data", legend=TRUE)
#' @seealso also \code{\link{pairs}}
mypairsSmooth = function(dataframe,legend=FALSE,...){
  pairs(dataframe, panel = function(x,y){
    graphics::smoothScatter(x, y, add=TRUE)
    graphics::abline(a=0,b=1,v=0,h=0,col=2)
    if(legend){
      cR2 <- stats::cor(x,y, use="pairwise.complete.obs")^2
      graphics::legend("topleft", legend=paste("R^2=", round(cR2, digits=2) , sep=""),text.col=3)}
  }
  , lower.panel=NULL,...
  )
}
