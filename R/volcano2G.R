. = NULL

#' DEPRECATED Volcano plot using ggplot and ggrepel
#' @param foldchange vector with fold changes
#' @param pvals vector with pvalues
#' @param labels vector with labels
#' @param pthresh pvalue threshold
#' @param log2FCThresh log2 FC threshold
#' @param main main title
#' @param xlab xlab
#' @param ylab ylab
#' @param xlim xlim
#' @param ylim ylim
#' @param size see geom_text_repel
#' @param segment.size see geom_text_repel
#' @param segement.alpha see geom_text_repel
#' @param pseudo usually q.mod containing NAs
#' @param colors specify colors for specific lables.
#' @import ggplot2
#' @import ggrepel
#' @examples 
#' rm(list=ls())
#' library(quantable)
#' foldchange <- rnorm(1000)
#' pvals <-rexp(1000)
#' volcano2G(foldchange, pvals,labels=rep("abcde", length(pvals)),
#'   pthresh=0.1, log2FCThresh=0.5,main='test',size=2,segment.size=0.3)
#' 
#' @export
volcano2G <- function(foldchange,
                       pvals,
                       labels,
                       pthresh=0.1,
                       log2FCThresh=0.5,
                       main=NULL,
                       xlab="log2 FC",
                       ylab="-log10(Q Value)",
                       xlim=c(-5,5),
                       ylim=c(0,-log10(min(pvals, na.rm=TRUE))),
                       size=1,
                       segment.size=0.3,
                       segement.alpha=0.3,
                       pseudo = NULL,
                       colors = NULL
                      )
{
  results <- data.frame(log2FoldChange = foldchange, pvalue= pvals, labels=labels )
  fcLabel <- paste("Q Value <", pthresh, "& |FC| >", log2FCThresh)
  
  if(is.null(results$significance)){
    results$significance = ifelse(results$pvalue < pthresh & abs(results$log2FoldChange) > log2FCThresh ,
                                  fcLabel ,"Not Sig" )
    if(!is.null(pseudo)){
      results$significance[is.na(pseudo)] <- "pseudo"
      colors <- c("black", "green", "red"   )
    }else{
      colors <- c("black", "red")
    }
  }
  
  ### hack to pass R CMD check
  log2FoldChange <- NULL
  pvalue <- NULL
  ### 
  p = ggplot(results, aes(log2FoldChange, -log10(pvalue))) +
    geom_point(aes_string(col="significance")) +
    scale_color_manual(values=colors)
  
  p = p + ggplot2::geom_hline(yintercept=-log10(pthresh), col=4, lty=2) 
  p = p + ggplot2::geom_vline(xintercept=c(-log2FCThresh,log2FCThresh), col=4,lty=2) 
  
  filtres <- subset(results, pvalue<pthresh & abs(log2FoldChange)>log2FCThresh )
  p = p + geom_text_repel(data=filtres, aes_string(label='labels'), size=size, segment.size = segment.size, segment.alpha = segement.alpha)
  if(!is.null(main)){
    p = p + ggtitle(main)
  }
  p = p + xlab(xlab)
  p = p + ylab(ylab)
  p = p + xlim(xlim[1],xlim[2])
  p = p + ylim(ylim[1],ylim[2])
  
  return(p)
}

#' Volcano with more control
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom ggrepel geom_text_repel
#' @param dataX dataX frame
#' @param foldchange column name with fold change plotted on X
#' @param pvalue column with pvalue or qvalue plotted as -log10 on y axes
#' @param labels column containing lables
#' @param pthresh horizontal abline
#' @param log2FCThresh vertical abline
#' @param main main plot title
#' @param xlab xlab
#' @param ylab ylab
#' @param repel.text.size ggrepel parameter
#' @param repel.segment.size ggrepel parameter
#' @param repel.segement.alpha ggrepel parameter
#' @param pseudo add pseudo fold changes
#' @examples
#' rm(list=ls())
#' 
#' library(ggrepel)
#' library(quantable)
#' foldchange <- rnorm(1000)
#' pvals <-rexp(1000)
#' names <- sample(colors(),1000,replace=TRUE)
#' 
#' dataX <- data.frame(q.mod = pvals,
#'  log2FC = foldchange,
#'   names = names )
#'   
#' volcano2GB(dataX)
#' b <- volcano2GB(dataX, pthresh=0.1, log2FCThresh=0.5 ,
#'  main='test', repel.segment.size=0.3,repel.text.size=2)
#' b
volcano2GB <- function(dataX, 
                      foldchange = "log2FC",
                      pvalue = "q.mod",
                      labels = "names",
                      pthresh=0.1,
                      log2FCThresh=0.5,
                      main=NULL,
                      xlab="log2 FC",
                      ylab="-log10(Q Value)",
                      repel.text.size=1,
                      repel.segment.size=0.5,
                      repel.segement.alpha=0.5,
                      pseudo= NULL
)
{
  dataX <- dataX %>% mutate(yvalue = -log10(!!rlang::sym(pvalue)))
  fcLabel <- paste(pvalue, "<", pthresh, "& |",foldchange,"| >", log2FCThresh)
  colors <- NULL
  if("significance" %in% colnames(dataX)){
    dataX$significance = ifelse(dataX[,pvalue] < pthresh & abs(dataX[,foldchange]) > log2FCThresh ,
                                  fcLabel ,"Not Sig" )
    if(!is.null(pseudo)){
      dataX$significance[is.na(pseudo)] <- "pseudo"
      colors <- c("black", "green", "red" )
    }else{
      colors <- c("black", "red")
    }
  }

  p = ggplot(dataX, aes_string(foldchange, "yvalue")) +
    geom_point(aes_string(col="significance"))
  if(!is.null(colors)){
    p = p + scale_color_manual(values=colors)
  }
  p = p + ggplot2::geom_hline(yintercept=-log10(pthresh), col=4, lty=2) 
  p = p + ggplot2::geom_vline(xintercept=c(-log2FCThresh,log2FCThresh), col=4,lty=2) 

  #cat("pthresh: " , pthresh, " log2FCThresh", log2FCThresh ,"\n")
  filtres <- dataX %>% filter( UQ(rlang::sym(pvalue)) < pthresh & abs( UQ(sym(foldchange) )) > log2FCThresh )
  p = p + geom_text_repel(data = filtres,
                          aes_string(label=labels),
                          size = repel.text.size,
                          segment.size = repel.segment.size,
                          segment.alpha = repel.segement.alpha)
  if(!is.null(main)){
    p = p + ggtitle(main)
  }
  p = p + xlab(xlab)
  p = p + ylab(ylab)
  
  return(p)
}

#' add special labels
#' @export
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr all_vars mutate_at filter_at funs
#' @param p ggplot2
#' @param dataX data.frame
#' @param special additional special labels for those entries in the labels column below.
#' @param foldchange name of fold change column
#' @param pvalue name of p-value column
#' @param labels name of labels column
#' @examples 
#' 
#' foldchange <- rnorm(1000)
#' pvals <-rexp(1000)
#' names <- sample(colors(),1000,replace=TRUE)
#'
#' dataX <- data.frame(
#'   q.mod = pvals,
#'   log2FC = foldchange,
#'   names = names
#' )
#' library(rlang)
#' foldchange = "log2FC"
#' p <- volcano2GB(dataX, pthresh=0.1, log2FCThresh=0.5 , main='test',
#'                 repel.segment.size=0.3,
#'                 repel.text.size=2)
#' special <- sample(colors(),5)
#' p <- addSpecialProteins(p, dataX, special)
#' p
#' 
addSpecialProteins <- function(p,
                               dataX,
                               special,
                               foldchange = "log2FC",
                               pvalue = "q.mod",
                               labels = "names"){
  #dataX <- dataX %>% mutate("yvalue" := -log10(UQ(sym(pvalue))))
  negLog10 <- function(x){-log10(x)}
  dataX <- dataX %>% mutate_at(c("yvalue" = pvalue), negLog10)
  testx <- function(x, special){tmp <- x %in% special; x[!tmp] <- NA; as.character(x)}
  dataX <- dataX %>% mutate_at(c("names2" = labels) , funs(testx(., special)))

  xx <- dataX %>% filter_at("names2",all_vars(!is.na(.)))
  if(nrow(xx) == 0){
    return(p)
  }
  p <- p + geom_point(data = xx, aes_string(foldchange, "yvalue"), color="cyan", shape=2)
  p <- p + geom_text_repel(data = dataX,
                           aes_string(label="names2"), color="blue")
  p
}


