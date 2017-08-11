#' plot volcano given multiple conditions
#' @param misspX data in long format
#' @param colour colouring of points
#' @param effect column containing effect sizes
#' @param type column containing p-values, q.values etc
#' @param Condition column with condition
#' @param xintercept fc thresholds
#' @param pvalue pvalue threshold
#' @param label column containing labels
#' @param size controls size of text
#' @param segment.size controls size of lines
#' @param segment.alpha controls visibility of lines
#' @param ablines adds ablines horizontal and vertical 
#' @importFrom ggrepel geom_text_repel
#' @export
#' @examples 
#' cc <- data.frame(
#' fc = c(0,0),
#' p = c(0.01,0.05), 
#' Area = c('p=0.01','p=0.05')
#' )
#' 
multigroupVolcano <- function(misspX,
                              colour,
                              effect = "fc",
                              type = "p.adjust",
                              Condition = "condition",
                              xintercept=c(-2,2),
                              pvalue=0.05,
                              label="row.names",
                              size=1
                              , segment.size = 0.3,
                              segment.alpha = 0.3, ablines = cc) {
  col = paste("-log10(", type , ")" , sep="")
  message(col)
  p <- ggplot( misspX, aes_string(x = effect , y = col  ))  + geom_point(col=colour, alpha=0.5)
  p <- p + facet_wrap(as.formula(paste("~",Condition))) + labs(y = col)
  p <- p + geom_abline(data = ablines, aes(slope = fc, intercept = -log10(p),colour = Area)) + 
    geom_vline(xintercept = xintercept,linetype = "dashed", colour = "red")
  if(!is.null(label)){
    message("test")
    effectX <-misspX[,effect]
    typeX<-misspX[,type]
    subsetData <- subset(misspX, (effectX < xintercept[1] | xintercept[2] < effectX) & typeX < pvalue )
    p <- p + geom_text_repel(data=subsetData, aes_string(effect , col , label=label), size=size
                             , segment.size = segment.size, segment.alpha = segment.alpha)
  }
  return(p)
}
