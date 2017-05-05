.getSequ<-function(length, increasing=TRUE){
  sequ <- NULL
  if(increasing){
    sequ <- seq(0, 1, length.out = length)
  }else{
    sequ <- seq(1, 0, length.out = length)
  }
  invisible(sequ)
}

#' create blue white red palette
#' @param  length nr of colors
#' @param increasing default TRUE
#' @export
#' @importFrom scales brewer_pal gradient_n_pal div_gradient_pal
#' @examples 
#' 
#' library(scales)
#' show_col(getBlueWhiteRed(21))
#' 
getBlueWhiteRed <- function(length=21, increasing = TRUE){
  div_gradient_pal(low="blue",mid="white",high="red")(.getSequ(length,increasing))
}
#' create divergent palette
#' @param  length nr of colors
#' @param pallete there are a few divergent palletes in brewer_pal, default 1
#' @param increasing default TRUE
#' @export
#' @examples 
#' 
#' library(scales)
#' show_col(getDiv(21,4))
#' show_col(getDiv(21,4, increasing=FALSE))
getDiv <- function(length = 11, pallete=3, increasing = TRUE){
  divcol <- brewer_pal(type="div", palette = pallete)(8)
  ygreen<-gradient_n_pal(divcol)(.getSequ(length,increasing))
  return(ygreen)
}
#' create green color scale
#' @param length nr of colors
#' @param increasing default FALSE
#' @export
#' @examples 
#' 
#' library(scales)
#' show_col(getGreensScale(5))
#' 
getGreensScale <- function(length = 10, increasing = FALSE){
  colsGreen <- brewer_pal(palette = "Greens")(8)
  ygreen<-gradient_n_pal(colsGreen)(.getSequ(length,increasing))
  return(ygreen)
}
#' create blue color scale
#' @param length nr of colors
#' @param increasing default FALSE
#' @export
#' @examples
#' 
#' library(scales)
#' show_col(getBlueScale(5))
#' 
getBlueScale <- function(length = 10, increasing = FALSE){
  colsBlue <- brewer_pal(palette = 1)(8)
  yblue<-gradient_n_pal(colsBlue)(.getSequ(length,increasing))
  return(yblue)
}

#' create red color scale
#' @param length nr of colors
#' @param increasing default FALSE
#' @export
#' @examples
#' 
#' library(scales)
#' show_col(getRedScale(12))
#' 
getRedScale <- function(length = 10, increasing = FALSE){
  colsRed <- brewer_pal(palette = 3)(5)
  yred<-gradient_n_pal(colsRed)(.getSequ(length,increasing))
  return(yred)
}
