# @TODO think of making it public.
#' copute jack knive
#' @param xdata matrix
#' @param .method method i.e. cor, parameters
#' @param ... further parameters to .method
#' @return list with all jackknife matrices
#' @export
#' @examples 
#' xx <- matrix(rnorm(20), ncol=4) 
#' cortest <- function(x){print(dim(x));cor(x)}
#' my_jackknife(xx, cortest)
#' my_jackknife(xx, cor, use="pairwise.complete.obs", method="pearson")
my_jackknife <- function ( xdata, .method, ...) {
  x <- 1:nrow(xdata)
  call <- match.call()
  n <- length(x)
  u <- vector( "list", length = n )
  for (i in 1:n) {
    tmp <- xdata[x[-i],]
    u[[i]] <- .method(tmp, ...)
  }
  names(u) <- 1:n
  thetahat <- .method(xdata, ...)
  invisible(list(thetahat = thetahat, jack.values = u, call = call ))
}

#' Compute correlation matrix with jack
#' @param dataX data.frame with transition intensities per peptide
#' @param distmethod dist or correlation method working with matrix i.e. cor
#' @param ... further parameters to method
#' @importFrom tidyr gather spread
#' @importFrom plyr ldply
#' @importFrom dplyr group_by summarize
#' @return summarizes results producced with my_jackknife
#' @export
#' @examples
#' dataX <- matrix(rnorm(20), ncol=4) 
#' rownames(dataX)<- paste("R",1:nrow(dataX),sep="")
#' colnames(dataX)<- paste("C",1:ncol(dataX),sep="")
#' tmp <- my_jackknife(dataX, cor, use="pairwise.complete.obs", method="pearson")
#' 
#' jackknifeMatrix(dataX, cor)
#' jackknifeMatrix(dataX, cor, method="spearman")
jackknifeMatrix <- function(dataX, distmethod , ... ){
  if(is.null(colnames(dataX))){
    colnames(dataX)<- paste("C",1:ncol(dataX),sep="")
  }
  if(is.null(rownames(dataX))){
    rownames(dataX)<- paste("R",1:nrow(dataX),sep="")
  }
  
  if(nrow(dataX) > 1 & ncol(dataX) > 1){
    tmp <- my_jackknife( dataX, distmethod, ... )
    x <- plyr::ldply(tmp$jack.values, quantable::matrix_to_tibble)
    dd <- tidyr::gather(x, "col.names" , "correlation" , 3:ncol(x))
    ddd <- dd %>% group_by(row.names, col.names) %>% summarize(jcor = max(correlation))
    
    dddd <- tidyr::spread(ddd, col.names, jcor  )
    dddd <- as.data.frame(dddd)
    rownames(dddd) <-dddd$row.names
    dddd <- dddd[,-1]
    return(dddd)
  }else{
    message("Could not compute correlation, nr rows : " , nrow(dataX) )
  }
}






