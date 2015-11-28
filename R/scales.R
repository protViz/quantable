#' create divergent palette
#' @param  length nr of colors
#' @export
#' @examples 
#' 
#' library(scales)
#' show_col(getDiv(21,4))
#' 
getDiv <- function(length = 10, pallete=3){
  divcol <- brewer_pal(type="div", palette = pallete)(8)
  ygreen<-gradient_n_pal(divcol)(seq(0, 1, length.out = length))
  return(ygreen)
}
#' create green color scale
#' @param  length nr of colors
#' @export
#' @examples 
#' 
#' library(scales)
#' show_col(getGreensScale(5))
#' 
getGreensScale <- function(length = 10){
  colsGreen <- brewer_pal(palette = "Greens")(8)
  ygreen<-gradient_n_pal(colsGreen)(seq(0, 1, length.out = length))
  return(ygreen)
}
#' create blue color scale
#' @param length nr of colors
#' @export
#' @examples
#' 
#' library(scales)
#' show_col(getBlueScale(5))
#' 
getBlueScale <- function(length = 10){
  colsBlue <- brewer_pal(palette = 1)(8)
  yblue<-gradient_n_pal(colsBlue)(seq(0, 1, length.out = length))
  return(yblue)
}

#' create red color scale
#' @param length nr of colors
#' @export
#' @examples
#' 
#' library(scales)
#' show_col(getRedScale(12))
#' 
getRedScale <- function(length = 10){
  colsRed <- brewer_pal(palette = 3)(5)
  yred<-gradient_n_pal(colsRed)(seq(0, 1, length.out = length))
  return(yred)
}
