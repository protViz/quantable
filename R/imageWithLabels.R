#' Copy of http://stackoverflow.com/questions/20977477/how-to-assign-a-specific-color-to-na-in-an-image-plot
#' 
#' @param z matrix
#' @param col color gradient
#' @param zlim range
#' @param na.color for displaying NA's.
#' @param outside.below.color color below zlim
#' @param outside.above.color color above zlim
#' @param breaks do not remember (see usage in imageWithLabelsNoLayout)
#' @param textB display numerical values of the matrix as text, default NULL no
#' @param text.cex size of numerical values
#' @param ... further parameters for plot method
#' @export
#' @importFrom reshape2 melt
#' 
image_nan <- function(z,
                      col =heat.colors(12),
                      zlim=NULL,
                      na.color='gray',
                      outside.below.color='green',
                      outside.above.color='green',
                      breaks,
                      textB=NULL,
                      text.cex=0.8, ...)
{
  # TODO: add checks for missing values
  print(na.color)
  z <- as.matrix(z)
  if(is.null(zlim)){
    zlim <- range(z,na.rm=TRUE)
  }
  zstep <- (zlim[2] - zlim[1]) / (length(col)-1); # step in the color palette
  newz.below.outside <- zlim[1] - zstep # new z for values below zlim
  newz.above.outside <- zlim[2] + zstep # new z for values above zlim
  newz.na <- zlim[2] + 2 * zstep # new z for NA
  
  z[which(z < zlim[1],arr.ind=TRUE)] <- newz.below.outside # we affect newz.below.outside
  z[which(z > zlim[2],arr.ind=TRUE)] <- newz.above.outside # we affect newz.above.outside
  
  z[which(is.na(z),arr.ind=TRUE)] <- newz.na # same for newz.na
  
  zlim[1] <- zlim[1] - zstep # extend lower limit to include below value
  zlim[2] <- zlim[2] + 2 * zstep # extend top limit to include the two new values above and na
  col <- c(outside.below.color, col, outside.above.color, na.color) # we construct the new color range by including: na.color and na.outside
  if(! missing(breaks)){
    breaks <- c( zlim[1],
                 breaks[1]- zstep/2,
                 breaks[2:(length(breaks)-1)],
                 breaks[length(breaks)]+zstep/2,
                 zlim[2]-zstep,
                 zlim[2] )
    
    image(z=z, zlim=zlim, col=col, breaks=breaks, ...) # we finally call image(...)
  }else{
    image(z=z, zlim=zlim, col=col, ...) # we finally call image(...)
  }
  if(!is.null(textB)){
    zz <- z
    rownames(zz) <- NULL
    colnames(zz) <- NULL
    zz <<- zz

    lmatrix <- reshape2::melt(zz)
    x <- ((lmatrix$Var1-1) /max(lmatrix$Var1))
    y <- ((lmatrix$Var2-1) /max(lmatrix$Var2))
    graphics::text(x * 1/max(x), y * 1/max(y),labels = round(lmatrix$value, digits=textB),cex = text.cex)
  }
}

#' image plot with labels
#'
#' @param x matrix
#' @param row.labels colnames(x)
#' @param col.labels rownames(x)
#' @param cex.axis size of axis lables
#' @param cex size of labels
#' @param main main title
#' @param col color map for matrix
#' @param digits number of digits on colorscale, default 2
#' @param xlab x label
#' @param ylab y label
#' @param zlim z value range, default NULL an determined from x
#' @param na.color na.color
#' @param textB indicate if correlation (text) should be added to heatmap. If - with how many digits. default = NULL (do not add).
#' @param ... passed to image
#' @export
#' @examples
#' x = matrix(rnorm(20*30),ncol=20)
#' rownames(x) <- 1:30
#' colnames(x) <- letters[1:20]
#' quantable:::image_nan(x,textB=1)
#' 
#' imageWithLabelsNoLayout(x,col = heat.colors(13),textB=2, text.cex=0.6)
#' imageWithLabelsNoLayout(x,col = heat.colors(12),breaks=seq(min(x),max(x),length=13))
#' x[3,3] <- NA
#' imageWithLabelsNoLayout(x,col = heat.colors(12),
#' breaks=seq(min(x,na.rm=TRUE),
#' max(x,na.rm=TRUE),length=13))
#' imageWithLabelsNoLayout(x,xlab="ttt",ylab="bbb", na.color="magenta")
#' imageWithLabelsNoLayout(x,xlab="ttt",ylab="bbb", zlim=c(0,2))
#' 
imageWithLabelsNoLayout = function(x,
                                   col.labels=colnames(x),
                                   row.labels=rownames(x),
                                   cex=1,
                                   cex.axis=0.5,main=NULL,
                                   col = heat.colors(12),
                                   digits=2,
                                   xlab='',
                                   ylab='',
                                   zlim=NULL,
                                   na.color='gray',
                                   textB=NULL,...){
  if(!is.null(zlim)){
    image_nan(x, axes = F, main = main, col=col,xlab=xlab, ylab=ylab, zlim=zlim,
              textB=textB,na.color = na.color, ...=...)
  }else{
    image_nan(x, axes = F, main = main, col=col,xlab=xlab, ylab=ylab, 
              textB=textB, na.color=na.color, ...=...)
  }
  graphics::axis( 2, at=seq(0,1,length=length((col.labels))) , labels=col.labels,cex.axis=cex.axis, las=2, cex=cex )
  graphics::axis( 1, at=seq(0,1,length=length((row.labels))) , labels=row.labels,cex.axis=cex.axis, las=2, cex=cex )
}
#' if you need an colorscale to you imagelables use this
#' @param x data the data matrix
#' @param col colors used
#' @param digits number of digits on color scale, default 2
#' @param cex cex
#' @param cex.axis cex.axis
#' @param zlim zlim
#' @param breaks optional argument passed to image (see image for more details)
#' @export
#' @examples
#' x = matrix(rnorm(20*30, 5),ncol=20)
#' rownames(x) <- 1:30
#' colnames(x) <- letters[1:20]
#' imageColorscale(x)
#' imageColorscale(x,col=getBlueWhiteRed(), zlim=c(-1,1))
#' imageColorscale(x,col=getBlueWhiteRed(), zlim=c(-5,5))
imageColorscale <- function(x, cex = 1,
                           cex.axis = 0.5,
                           col = heat.colors(12),
                           digits=2,
                           zlim=NULL, breaks ){
  if(!is.null(zlim)){
    colorlevels = seq(zlim[1],zlim[2],length=length(col))
    
        image(1, seq(0,1,length=length(colorlevels)),
          matrix(data=colorlevels, nrow=1),
          col=col,xlab="",ylab="",
          axes=FALSE,zlim=zlim)
  }else{
    colorlevels = seq(min(x,na.rm = TRUE),max(x,na.rm = TRUE),length=length(col))
    
    image(1, seq(0,1,length=length(colorlevels)),
          matrix(data=colorlevels, nrow=1),
          col=col,xlab="",ylab="",
          axes=FALSE)
  }
  axis( 2,
        at=seq(0,1,length=length((colorlevels))) ,
        labels=round(colorlevels,digits=digits),
        cex.axis=cex.axis,
        las=1,
        cex=cex )
}

#' image plot with labels
#'
#' @param x matrix
#' @param row.labels colnames(x)
#' @param col.labels rownames(x)
#' @param cex.axis size of axis lables
#' @param cex size of labels
#' @param main main title
#' @param col color map for matrix
#' @param digits number of digits on colorscale, default 2
#' @param marLeft margins of left image see ?par for more detail
#' @param marRight margins of right image see ?par for more detail
#' @param xlab x label
#' @param ylab y label
#' @param zlim z value range, default NULL an determined from x
#' @param na.color na.color
#' @param widths controls the size of left and right pane
#' @param ... passed to image
#' @export
#' @examples
#' x = matrix(rnorm(20*30),ncol=20)
#' rownames(x) <- 1:30
#' colnames(x) <- letters[1:20]
#' martmp <- par()$mar
#' imageWithLabels(x)
#' imageWithLabels(x,marLeft = c(5,5,2,2),marRight=c(0,0,0,0),xlab="ttt",ylab="bbb")
#' par(mar = martmp)
#' 
imageWithLabels <- function(x,
                           col.labels=colnames(x),
                           row.labels=rownames(x),
                           cex=1,
                           cex.axis=0.5,
                           main=NULL,
                           col = heat.colors(12),
                           digits=2,
                           marLeft= graphics::par()$mar,
                           marRight = graphics::par()$mar,
                           xlab='',
                           ylab='',
                           zlim=NULL,
                           na.color='gray',widths=c(4,1),...)
{
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=widths, heights=c(1,1))
  graphics::par(mar=marLeft)
  imageWithLabelsNoLayout(x,col.labels=col.labels,
                          row.labels=row.labels,
                          cex=cex,
                          cex.axis=cex.axis,
                          main=main,
                          col = col,
                          digits=digits,
                          xlab=xlab,
                          ylab=ylab,
                          zlim=zlim,
                          na.color=na.color,...=...
  )
  
  graphics::par(mar=marRight)
  imageColorscale(x, cex = cex, cex.axis = cex.axis,col = col, digits=digits, zlim=zlim)  
  layout(1)
}

