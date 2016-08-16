#' get NR of NA's per matrix or data.frame row
#' 
#' @param x matrix or data.frame
#' @export
rowNAs <- function(x){
  return(apply(x, 1, function(x){sum(is.na(x))}))
}
#' get NR of NA's per matrix or data.frame column
#' 
#' @param x matrix or data.frame
#' @export
colNAs <- function(x){
  return(apply(x, 2, function(x){sum(is.na(x))}))
}