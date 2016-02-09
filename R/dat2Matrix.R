#' 
#' converts sparse representation to dense where row and col can be character vectors
#' @export
#' @param row row positions
#' @param col column positions
#' @param x  values for matrix
#' @return matrix
dat2Matrix <- function(row,col,x){
  stopifnot(length(row) == length(col), length(x) == length(col))
  rowf <- as.factor(row)
  colf <- as.factor(col)
  rowi <- as.integer(rowf)
  coli <- as.integer(colf)
  x<- Matrix::sparseMatrix(rowi, coli, x=x)
  x<-as.matrix(x)
  rownames(x) <- levels(rowf)
  colnames(x) <- levels(colf)
  return(x)
}
