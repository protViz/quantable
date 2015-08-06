#' running robust scaling of arefw
#' @export
#' @param arefw - data array to scale
#' @param k - windows
#' @param scale - should also scaling be applied
#' @return list with scaled data runmed used to center the data and runmad the running MAD used for scaling
#' @examples
#' a <- t(replicate(200,rnorm(20,runif(1,-3,3),1)))
#' b <- a[1:100,]
#' a <- a[101:200,]
#' boxplot(t(a[1:20,]))
#' boxplot(t(b[1:20,]))
#' res <- getValuesForVolcano(a,b)
#' volcanoplot(res$fchange , res$pval)
getValuesForVolcano=function(x,y){
  pval = rep(NA, nrow(x))
  fchange = rep(NA, nrow(x))
  for(i in 1:nrow(x)){
    tmp <- t.test(x[i,],y[i,])
    pval[i] <- tmp$p.value
    fchange[i] <-tmp$estimate[1] - tmp$estimate[2]
  }
  return(list(pval= p.adjust(pval, method="BH"), fchange=fchange))
}



