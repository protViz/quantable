#' get values of upper triangle from matrix
#' @export
#' @param mat matrix
#' @param diag default = FALSE
#' @examples
#'
#' t = matrix(1:25,ncol=5)
#' uppertriang(t)
uppertriang <- function(mat,diag=FALSE){
  res<-mat[upper.tri(mat,diag=diag)]
  return( c(unlist(res)) )
}
