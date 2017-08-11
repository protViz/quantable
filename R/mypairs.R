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
#' histogram panel for pairs function (used as default in mypairsSmooth)
#' @export
#' @param x numeric data
#' @param ... additional parameters passed to rect
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,  ...)
}
#' correlation panel for pairs plot function (used as default in mypairsSmooth)
#' @export
#' @param x numeric data
#' @param y numeric data
#' @param ... not used
#' @param digits number of digits to display
panel.cor <- function(x, y, digits = 2, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y, use="pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.7, txt)
  
  txt <- format(c(r^2, 0.123456789), digits = digits)[1]
  txt <- bquote(R^{2} ~"=" ~ .(txt))
  text(0.5, 0.5, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}

#' smoothScatter pairs
#' @param dataframe data matrix or data.frame as normally passed to pairs
#' @param legend  add legend to plots
#' @param ... params usually passed to pairs
#' @export
#' @examples
#' tmp = matrix(rep((1:100),times = 4) + rnorm(100*4,0,3),ncol=4)
#' mypairsSmooth(tmp,main="small data", legend=TRUE)
#' mypairsSmooth(tmp,main="small data", diag.panel=panel.hist)
#' mypairsSmooth(tmp,log="xy",main="small data", legend=TRUE)
#' @seealso also \code{\link{pairs}}
mypairsSmooth = function(dataframe, legend=FALSE, ...){
  pairs(dataframe, upper.panel = function(x,y){
    graphics::smoothScatter(x, y, add=TRUE)
    graphics::abline(a=0,b=1,v=0,h=0,col=2)
    if(legend){
      cR2 <- stats::cor(x,y, use="pairwise.complete.obs")^2
      graphics::legend("topleft",
                       legend=paste("R^2=", round(cR2, digits=2) ,
                                    sep=""),text.col=3)}
  }
  , lower.panel=panel.cor, ...
  )
}
