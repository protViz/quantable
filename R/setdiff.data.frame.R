#' setdiff for data frames
#' @export
#' @param x data.frame
#' @param y data.frame
setdiff_data.frame <- function(x, y){ x[ !duplicated( rbind(y,x) )[ -seq_len(nrow(y))] , ] }
