#' plot volcano given multiple conditions
#' @param misspX data in long format
#' @param effect column containing effect sizes
#' @param type column containing p-values, q.values etc
#' @param Condition column with condition
#' @param xintercept fc thresholds
#' @param pvalue pvalue threshold
#' @param label column containing labels
#' 
#' @export
#' 
multigroupVolcano <- function(misspX,
                        effect = "fc",
                        type = "p.adjust",
                        Condition = "condition",
                        xintercept=c(-2,2),
                        pvalue=0.05, label="row.names") {
  col = paste("-log10(", type , ")" , sep="")
  message(col)
  p <- ggplot( misspX, aes_string(x = effect , y = col  ))  + geom_point()
  p <- p + facet_wrap(as.formula(paste("~",Condition))) + labs(y = col)
  p <- p + geom_abline(data = cc, aes(slope = sl, intercept = -log10(p),colour = Area)) + 
    geom_vline(xintercept = xintercept,linetype = "dashed", colour = "red")
  if(0){
    effectX <-misspX[,effect]
    typeX<-misspX[,type]
    subsetData <- subset(misspX, (effectX < xintercept[1] | xintercept[2] < effectX) & typeX < pvalue )
    p <- p + geom_text(data=subsetData, aes_string(effect , col , label=label))
  }
  return(p)
}
