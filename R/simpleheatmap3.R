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
#' @param nrOfClusters number of distinct clusters for cutree function. Default is 3
#' @param ... other parameters to heatmap3
#' @examples
#' df <- matrix(rnorm(150), ncol = 10) # 15x10 matrix
#' clust <- simpleheatmap3(pln = df, main = "", distf = dist, hclustf = hclust, labRow = "", plot = F, nrOfClusters = 3) 
#' # plot = F will return a data.frame
#' # with the specified number of distinct clusters in nrOfClusters
#' hmp3 <- simpleheatmap3(pln = df, main = "", distf = dist, hclustf = hclust, labRow = "", plot = F, nrOfClusters = 3)  
#' plot = T will plot the heatmap with row and column dendrogram

simpleheatmap3 <- function(pln,
                           main="",
                           distf=dist,
                           hclustf=hclust,
                           labRow="",
                           labCol="",
                           palette=getBlueWhiteRed(),
                           margins=c(5,5),scale="none",
                           plot = TRUE,
                           nrOfClusters = 3, ...)
{
  if(plot) {
    tmp <- heatmap3( as.matrix(pln) , scale=scale  , col=palette ,
              labRow=labRow,
              cexRow=0.1 + 1/log10(dim(pln)[1]),
              cexCol=0.1 + 1/log10(dim(pln)[2]),
              distfun=distf,hclustfun=hclustf,
              margins=margins,main=main,...=...)
    clusterIDs <- cutree(tmp$hcc, nrOfClusters)
    return(data.frame(rowID = labCol, clusterID = clusterIDs))
  }
  else {
    tmp <- hclustf(distf(t(as.matrix(pln))))
    clusterIDs <- cutree(tmp, nrOfClusters)
    return(data.frame(rowID = labCol, clusterID = clusterIDs))
  }
  
}
