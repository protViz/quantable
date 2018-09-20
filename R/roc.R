#' create density plots of cases and controls and ROC plot
#' 
#' @param cases  a factor of predicted classes 
#' @param controls  a factor of classes to be used as the true results
#' @param label image main label
#' @param xlab label for x axis
#' @param xlim x axis extend
#' @param abline draws vertical abline to indicate best threshold
#' @importFrom pROC roc
#' @export
#' @examples 
#' library(pROC)
#' cases <- rnorm(100,-1,1.5)
#' controls <- rnorm(300,1,1.5)
#' makeROCplot(cases,controls)
#' 
makeROCplot <- function(cases,controls,label="",
                        xlab="P(X==1)",xlim=NULL,abline= NULL){
  graphics::par(mfrow=c(1,2))
  ll = range(c(cases,controls))
  if(is.null(xlim)){
    xlim = ll
  }
  graphics::par(mar=c(4,4,2,2))
  dt = stats::density(cases)
  dm = stats::density(controls)
  y = c(dt$y, dm$y)
  graphics::plot(dt, col=1,xlab=xlab,main=label, xlim=xlim, ylim=c(0,max(y)))
  lines(dm, col=2)
  if(!is.null(abline)){
    graphics::abline(v=abline,col=3)
  }
  rtmp <- pROC::roc(controls=controls, cases=cases)
  graphics::plot(rtmp)
  graphics::legend("bottomright",legend=round(rtmp$auc,digits=3))
}

