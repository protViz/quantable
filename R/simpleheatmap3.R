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
#' @param suppressColSideCols Option to suppress color side labelling per default
#' @param ... other parameters to heatmap3
#' @examples
#' df <- matrix(rnorm(150), ncol = 10) # 15x10 matrix without NAs
#' 
#' clust <- simpleheatmap3(pln = df,
#'                         main = "",
#'                         distf = dist,
#'                         hclustf = hclust,
#'                         labRow = "",
#'                         plot = FALSE,
#'                         nrOfClustersRow = 3,
#'                         suppressColSideCols = TRUE) 
#' # plot = F will return a list containing two data.frames 
#' # with the specified number of distinct clusters in nrOfClustersRow
#' # and nrOfClustersCol
#' 
#' hmp3 <- simpleheatmap3(pln = df,
#'                        main = "",
#'                        distf = dist,
#'                        hclustf = hclust,
#'                        labRow = "",
#'                        plot = TRUE,
#'                        nrOfClustersRow = 3, 
#'                        nrOfClustersCol = 3, 
#'                        suppressColSideCols = FALSE)
#' # plot = TRUE will result in the same list and additionally plot the heatmap. 
#' suppressColSideCols = FALSE #' will leave the heatmap with ColSideColors
#' 
#' df[3,1:7] <- NA # Seed some missing values in line 3
#' 
#' hmp3_withNAs <- simpleheatmap3(pln = df,
#'                        main = "",
#'                        distf = dist,
#'                        hclustf = hclust,
#'                        labRow = "",
#'                        plot = TRUE,
#'                        nrOfClustersRow = 3, 
#'                        nrOfClustersCol = 3, 
#'                        suppressColSideCols = FALSE)
#' # Since line 3 contained more than ncol(df)/2 missing values 
#' it is removed from the clustering.
#' 

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
                           nrOfClustersRow = 3,
                           suppressColSideCols = FALSE, ...)
{
  pln <- as.matrix(pln[rowNAs(pln) < (ncol(pln)/2),])
  if(plot) {
    tmp0 <- cutree(hclustf(distf(t(as.matrix(pln)))), nrOfClustersCol)
    colsidecolors <- rainbow(nrOfClustersCol)[tmp0]
    tmp <- heatmap3::heatmap3(as.matrix(pln), scale=scale, col=palette,
                              labRow=labRow,
                              labCol = labCol,
                              cexRow=0.1 + 1/log10(dim(pln)[1]),
                              cexCol=0.1 + 1/log10(dim(pln)[2]),
                              distfun=distf,hclustfun=hclustf,
                              margins=margins,main=main,
                              keep.dendro = TRUE,
                              ColSideLabs = "",
                              RowSideLabs = "",
                              ColSideColors = get(ifelse(suppressColSideCols, NULL, "colsidecolors")),
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
