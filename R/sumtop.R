#' sums top rows of matrix
#' uses median of row to determine row order
#' @export
#' @param x a matrix
#' @param top how many to rows to sum (default 3)
#' @examples
#' tmp <- rbind(rep(1,times = 4), rep(2,times=4), rep(3,times=4), rep(4,times=4))
#' res <- sumtop(tmp)
#' stopifnot(res == c(9,9,9,9))
#' 
sumtop <- function( x , top=3 ){
  if(nrow(x) > top){
    topN = min(nrow(x),top)
    medrow <- apply(x, 1 , median)
    ord<-order(medrow, decreasing = TRUE)[1:topN]
    x<-x[ord,]
  }
  return(apply(x,2,sum,na.rm=TRUE))
}
