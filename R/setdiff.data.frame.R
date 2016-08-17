#'setdiff for data frames
#'@param A data.frame
#'@param B data.frame
#'@export
setdiff.data.frame <- function(A,B){ A[ !duplicated( rbind(B,A) )[ -seq_len(nrow(B))] , ] }
