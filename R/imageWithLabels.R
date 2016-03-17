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
#' @export
#' @examples
#' x = matrix(rnorm(20*30),ncol=20)
#' rownames(x) <- 1:30
#' colnames(x) <- letters[1:20]
#' imageWithLabelsNoLayout(x)
#' imageWithLabelsNoLayout(x,xlab="ttt",ylab="bbb")
#' 
imageWithLabelsNoLayout = function(x, col.labels=colnames(x), row.labels=rownames(x), cex=1,cex.axis=0.5,main=NULL,
                           col = heat.colors(12), digits=2, xlab='',ylab='', zlim=NULL)
{
  if(!is.null(zlim)){
    image(x, axes = F, main =main, col=col,xlab=xlab,ylab=ylab,zlim=zlim)
  }else{
    image(x, axes = F, main =main, col=col,xlab=xlab,ylab=ylab)
  }
  
  axis( 2, at=seq(0,1,length=length((col.labels))) , labels=col.labels,cex.axis=cex.axis, las=2, cex=cex )
  axis( 1, at=seq(0,1,length=length((row.labels))) , labels=row.labels,cex.axis=cex.axis, las=2, cex=cex )
}
#' if you need an colorscale to you imagelables use this
#' @param x data the data matrix
#' @param col colors used
#' @param digits number of digits on color scale, default 2
#' @param cex cex
#' @param cex.axis cex.axis
#' @param zlim zlim
#' @export
#' @examples
#' 
#' x = matrix(rnorm(20*30),ncol=20)
#' rownames(x) <- 1:30
#' colnames(x) <- letters[1:20]
#' colorscale(x)
imageColorscale = function(x, cex = 1, cex.axis = 0.5,col = heat.colors(12), digits=2, zlim=NULL){
  colorlevels = seq(min(x,na.rm = TRUE),max(x,na.rm = TRUE),length=length(col))
  if(!is.null(zlim)){
    image(1, seq(0,1,length=length(colorlevels)),
          matrix(data=colorlevels, nrow=1),
          col=col,xlab="",ylab="",
          axes=FALSE,zlim=zlim)
  }else{
    image(1, seq(0,1,length=length(colorlevels)),
          matrix(data=colorlevels, nrow=1),
          col=col,xlab="",ylab="",
          axes=FALSE)
  }
  axis( 2, at=seq(0,1,length=length((colorlevels))) , labels=round(colorlevels,digits=digits),cex.axis=cex.axis, las=1, cex=cex )
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
imageWithLabels = function(x, col.labels=colnames(x), row.labels=rownames(x), cex=1,cex.axis=0.5,main=NULL,
                           col = heat.colors(12), digits=2, marLeft=par()$mar,
                           marRight = par()$mar, xlab='',ylab='', zlim=NULL)
{
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(3,1), heights=c(1,1))
  par(mar=marLeft)
  imageWithLabelsNoLayout(x,col.labels=col.labels, row.labels=row.labels, cex=cex,cex.axis=cex.axis,main=main,
                          col = col, digits=digits, xlab=xlab,ylab=ylab, zlim=zlim)
  par(mar=marRight)
  imageColorscale(x, cex = cex, cex.axis = cex.axis,col = col, digits=digits, zlim=zlim)  
  layout(1)
}

