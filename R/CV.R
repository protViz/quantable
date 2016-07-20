#' compute CV for each row in matrix
#' 
#' Typically used to create and violin plot
#' @param data matrix
#' @param top remove top (default 30) CV
#' @param na.rm default TRUE
#' @export
#' @examples
#' dat <- matrix(rnorm(1000,10,5), ncol=20)
#' dim(dat)
#' cv <- CV(dat, top=5)
#' length(cv)
#' stopifnot(length(cv) == 45)
#' hist(cv)
CV <- function(data, top = 30, na.rm = TRUE){
  # TODO review code - there might to many checks and filters for NA
  idx <- apply(data,1, function(x){(ncol(data) - sum(is.na(x))) >= 2 })
  data <- data[idx,]
  sd = apply(data, 1, sd, na.rm = na.rm)
  mean = apply(data, 1, mean, na.rm = na.rm)
  idx <- mean==0 | is.na(mean)
  sd <- sd[!idx]
  mean <- mean[!idx]
  res = sd/mean * 100
  xx <- rank(res)
  res <- res[xx<=(length(xx)-top)]
  return(res)
}
#' geometric coefficient of variation (CV for log transformed data)
#' Typically used to create and violin plot
#'
#' @param data matrix
#' @param top remove top (default 30) CV
#' @export
#' @seealso CV
#' @examples
#' dat <- matrix(rnorm(1000,10,5), ncol=20)
#' dim(dat)
#' cv <- CVlog(dat, top=5)
#' length(cv)
#' stopifnot(length(cv) == 45)
#' hist(cv)
CVlog <- function(data,top=30){
  sd=apply(data,1,sd, na.rm=TRUE)
  res = (exp(sd)-1)
  xx <- rank(res)
  res <- res[xx<=(length(xx)-top)]
  return(res)
}
