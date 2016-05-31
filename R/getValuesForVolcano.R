#' get p-values of t-test values for volcano
#' @export
#' @param x - one data matrix
#' @param y - second data matrix
#' @param alternative two.sided, less, greater
#' @return list with three fields fchange (fold change) , pval and pvaladj
#' @examples
#' a <- t(replicate(200,rnorm(20,runif(1,-3,3),1)))
#' b <- a[1:100,]
#' a <- a[101:200,]
#' boxplot(t(a[1:20,]))
#' boxplot(t(b[1:20,]))
#' res <- getTValuesForVolcano(a,b)
#' volcanoplot(res$fchange , res$pval)
getTValuesForVolcano <- function(x, alternative="two.sided"){
  stopifnot(nrow(x) == nrow(y))
  pval = rep(NA, nrow(x))
  fchange = rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    tmp <- t.test(x[i,],y[i,], paired = FALSE, alternative=alternative)
    pval[i] <- tmp$p.value
    fchange[i] <-tmp$estimate[1] - tmp$estimate[2]
  }
  pvaladj <- p.adjust(pval, method="BH")
  return(list(pval= pval, pvaladj = pvaladj, fchange=fchange))
}
#' get p-values of wilcoxon rank sum test for volcano
#' @export
#' @param x - one data matrix
#' @param y - second data matrix
#' @param paired a logical indicating whether you want a paired t-test.
#' @param adjust pvalues using Benjamin Hochberg
#' @return list with two fields fchange (fold change) and pval
#' @examples
#' a <- t(replicate(200,rnorm(20,runif(1,-3,3),1)))
#' b <- a[1:100,]
#' a <- a[101:200,]
#' boxplot(t(a[1:20,]))
#' boxplot(t(b[1:20,]))
#' res <- getWRValuesForVolcano(a,b)
#' volcanoplot(res$fchange , res$pval)
getWRValuesForVolcano <- function(x,y, paired = FALSE, adjust=TRUE){
  stopifnot(nrow(x) == nrow(y))
  pval = rep(NA, nrow(x))
  fchange = rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    xv<-as.numeric(x[i,])
    yv<-as.numeric(y[i,])
    tmp <- wilcox.test(xv,yv, paired = paired)
    pval[i] <- tmp$p.value
    fchange[i] <- median( xv) - median(yv)
  }
  if(adjust){
    pval <- p.adjust(pval, method="BH")
  }
  return(list(pval= pval, fchange=fchange))
}
