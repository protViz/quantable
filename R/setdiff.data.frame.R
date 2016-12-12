#'setdiff for data frames
#' @param x data.frame
#' @param y data.frame
#' @param ... match signature of generic setdiff
#' @method setdiff data.frame
#' @export
setdiff.data.frame <- function(x, y,...){ x[ !duplicated( rbind(y,x) )[ -seq_len(nrow(y))] , ] }
