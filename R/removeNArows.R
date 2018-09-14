#' Removes rows with more than thresh NA's from matrix
#' @export
#' @return matrix
#' @param obj matrix or dataframe
#' @param thresh - maximum number of NA's / row - if more the row will be removed
#' @examples
#'
#' obj = matrix(rnorm(10*10),ncol=10)
#' dim(obj)
#' obj[3,3] = NA
#' x1 = removeNArows(obj, thresh=0)
#' stopifnot(all(c(9,10)==dim(x1)))
#' x2 = removeNArows(obj, thresh=1)
#' stopifnot(all(c(10,10)==dim(x2)))
removeNArows <- function(obj, thresh=0 )
{
  x <- apply(obj,1,function(x){sum(is.na(x))})
  obj <- obj[!(x>thresh),]
}
