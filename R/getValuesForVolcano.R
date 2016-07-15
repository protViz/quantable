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
getTValuesForVolcano <- function(x,y, alternative="two.sided"){
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
    tmp <- stats::wilcox.test(xv,yv, paired = paired)
    pval[i] <- tmp$p.value
    fchange[i] <- median( xv) - median(yv)
  }
  if(adjust){
    pval <- p.adjust(pval, method="BH")
  }
  return(list(pval= pval, pvaladj =  p.adjust(pval,method="BH") , fchange=fchange))
}
#' get p-values using fishers exact test for count data
#' @export
#' @param x - array
#' @param y - array
#' @param accessions accession string
#' 
#' @return data frame with accessions, pval, pvaldj (BH adjusted p.values), fchange (log2 FC).
#' 
#' @examples 
#' accessions <- letters
#' x <- sample(100,length(letters))
#' y <- sample(100,length(letters))
#' res <- fisherExact(x,y,accessions)
#' volcanoplot(res$fchange, res$pvaladj, labels = res$accessions)
#' 
fisherExact <- function(x, y, accessions){
  All <- sum(x)
  Bll <- sum(y)
  res <- vector(length(x), mode="list")
  fchange = rep(NA, length(x))
  for(i in 1:length(x)){
    A <- x[i]
    nA <- All - A
    B <- y[i]
    nB <- Bll - B
    accession <- accessions[i]
    C <- matrix( c(A, B, nA, nB),
                 nrow=2,
                 byrow=T,
                 dimnames=list(protein=c(accession,paste("not", accession)),
                               condition = c("control", "treated")))
    res[[i]]<-stats::fisher.test(C)
    fchange[i] <- log2(1+A) - log2(1+B)
  }
  p.value <- sapply(res, function(x){x$p.value})
  p.value.adjust <- p.adjust(p.value, method="BH")
  return(data.frame(accessions = accessions, pval=p.value, pvaladj= p.value.adjust, fchange = fchange))
}

