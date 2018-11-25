#' plot volcano given multiple conditions
#' @param misspX data in long format
#' @param effect column containing effect sizes
#' @param type column containing p-values, q.values etc
#' @param condition column with condition
#' @param colour colouring of points
#' @param xintercept fc thresholds
#' @param pvalue pvalue threshold
#' @param label column containing labels
#' @param size controls size of text
#' @param segment.size controls size of lines
#' @param segment.alpha controls visibility of lines
#' @param ablines adds ablines horizontal and vertical 
#' @param scales parameter to ggplot2::facet_wrap
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats as.formula
#' @import ggplot2
#' @export
#' @examples 
#' library(ggplot2)
#' library(tidyverse)
#' library(ggrepel)
#' data(multigroupFCDATA)
#' colnames(multigroupFCDATA)
#' multigroupVolcano(multigroupFCDATA,effect="logFC",type="adj.P.Val",condition="Condition",colour="colour",label="Name" )
multigroupVolcano <- function(misspX,
                              effect = "fc",
                              type = "p.adjust",
                              condition = "condition",
                              colour = "colour",
                              xintercept=c(-2,2),
                              pvalue=0.05,
                              label=NULL,
                              size=1
                              , segment.size = 0.3,
                              segment.alpha = 0.3,
                              ablines = data.frame(
                                fc=c(0,0),
                                p = c(0.01,0.05), 
                                Area = c('p=0.01','p=0.05')
                              ), scales="fixed",
                              maxNrOfSignificantText = 20) 
{
  colname = paste("-log10(", type , ")" , sep="")
  p <- ggplot( misspX, aes_string(x = effect , y = colname, color=colour  )  )  +
    geom_point(alpha=0.5)
  p <- p + scale_colour_manual(values=c("black", "green", "blue","red"))
  p <- p + facet_wrap(as.formula(paste("~",condition)),scales=scales) + labs(y = colname)
  
  ablines$neg_log10p <- -log10(ablines$p)
  p <- p + geom_abline(data = ablines, aes_string(slope = "fc", intercept = "neg_log10p",colour = "Area")) + 
    geom_vline(xintercept = xintercept,linetype = "dashed", colour = "red")
  
  if(!is.null(label)){
    effectX <-misspX[,effect]
    typeX<-misspX[,type]
    subsetData <- subset(misspX, (effectX < xintercept[1] | xintercept[2] < effectX) & typeX < pvalue ) %>% head(maxNrOfSignificantText)
    if(nrow(subsetData) > 0){
      p <- p + geom_text_repel(data=subsetData, aes_string(effect , colname , label=label),
                               size=size
                               , segment.size = segment.size,
                               segment.alpha = segment.alpha)
    }
  }
  return(p)
}


