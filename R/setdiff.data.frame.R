#' setdiff for data frames
#' @param x data.frame
#' @param y data.frame
#' @param ... match signature of generic setdiff
#' @export
setdiff.data.frame <- function(x, y,...){ x[ !duplicated( rbind(y,x) )[ -seq_len(nrow(y))] , ] }
