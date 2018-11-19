#' heatmap3 facade
#' @export
#' @importFrom heatmap3 heatmap3
#' @param pln matrix or dataframe with numerical values
#' @param distf distance function
#' @param hclustf clustering function
#' @param palette color palette
#' @param main title
#' @param labRow row labels
#' @param labCol column labels
#' @param margins control margins of heatmap
#' @param scale c(row, column or none)
#' @param plot logical; If TRUE simpleheatmap3 returns modified heatmap3 object, if FALSE returns a data.frame with clusterIDs. Default is TRUE
#' @param nrOfClustersRow number of distinct clusters for cutree function on rows. Default is 3
#' @param nrOfClustersCol number of distinct clusters for cutree function on columns. Default is 3
#' @param ... other parameters to heatmap3
#' @examples
#' df <- matrix(rnorm(150), ncol = 10) # 15x10 matrix
#' clust <- simpleheatmap3(pln = df,
#'  main = "",
#'  distf = dist,
#'  hclustf = hclust,
#'  labRow = "",
#'  plot = FALSE,
#'  nrOfClustersRow = 3) 
#' # plot = F will return a list containing two data.frames 
#' # with the specified number of distinct clusters in nrOfClustersRow
#' # and nrOfClustersCol
#' hmp3 <- simpleheatmap3(pln = df,
#'  main = "",
#'  distf = dist,
#'  hclustf = hclust,
#'  labRow = "",
#'  plot = FALSE,
#'  nrOfClustersRow = 3)  


simpleheatmap3 <- function(pln,
                           main="",
                           distf=dist,
                           hclustf=hclust,
                           labRow="",
                           labCol="",
                           palette=getBlueWhiteRed(),
                           margins=c(5,5),scale="none",
                           plot = TRUE,
                           nrOfClustersCol = 3,
                           nrOfClustersRow = 3, ...)
{
  pln <- na.omit(as.matrix(pln))
  if(plot) {
    tmp <- heatmap3::heatmap3( pln , scale=scale  , col=palette ,
              labRow=labRow,
              labCol = labCol,
              cexRow=0.1 + 1/log10(dim(pln)[1]),
              cexCol=0.1 + 1/log10(dim(pln)[2]),
              distfun=distf,hclustfun=hclustf,
              margins=margins,main=main,
              keep.dendro = TRUE,
              ...=...)
    clusterIDsRow <- cutree(as.hclust(tmp$Rowv), nrOfClustersRow)
    clusterIDsCol <- cutree(as.hclust(tmp$Colv), nrOfClustersCol)
    return(list(Row = data.frame(rowID = labRow,
                                 clusterID = clusterIDsRow,
                                 stringsAsFactors = F),
                Col = data.frame(colID = labCol,
                                 clusterID = clusterIDsCol,
                                 stringsAsFactors = F)))
  }
  else {
    tmp <- hclustf(distf(t(as.matrix(pln))))
    clusterIDsCol <- cutree(tmp, nrOfClustersCol)
    tmp <- hclustf(distf(as.matrix(pln)))
    clusterIDsRow <- cutree(tmp, nrOfClustersRow)
    return(list(Row = data.frame(rowID = labRow,
                                 clusterID = clusterIDsRow,
                                 stringsAsFactors = F),
                Col = data.frame(colID = labCol,
                                 clusterID = clusterIDsCol,
                                 stringsAsFactors = F)))
  }
  
}
