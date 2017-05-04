#' create density plots of cases and controls and ROC plot
#' 
#' @param cases  a factor of predicted classes 
#' @param controls  a factor of classes to be used as the true results
#' @param label image main label
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
makeROCplot <- function(cases,controls,label="",xlab="P(X==1)",xlim=NULL,abline= NULL){
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
  rtmp<-pROC::roc(controls=controls, cases=cases)
  graphics::plot(rtmp)
  graphics::legend("bottomright",legend=round(rtmp$auc,digits=3))
}
#' determine best Accuracy cuttoff
#' 
#' @param cases a factor of predicted classes 
#' @param controls a factor of classes to be used as the true results
#' @param plot create plot of threasholds versus Accuracy
#' @param scanstep step size for threshold estimation
#' @importFrom caret confusionMatrix
#' @export
#' @examples 
#' library(pROC)
#' cases <- rnorm(100,-1,1.1)
#' controls <- rnorm(500,1,1.1)
#' cut <- determineCut(cases,controls)
#' makeROCplot(cases,controls,abline=cut)
#' 
determineCut<-function(cases, controls , plot=FALSE, scanstep=0.01){
  data <- c(cases, controls)
  references <- c(rep(1, length(cases)), rep(0, length(controls)))
  thresh <- seq(min(data)+ scanstep*5 , max(data) - scanstep*5, by=scanstep)
  accur <- NULL
  for(thr in thresh){
    tmp <- caret::confusionMatrix(as.numeric(data < thr), references )
    accur <- c(accur,tmp$overall["Accuracy"])
  }
  cut <-thresh[which.max(accur)]
  if(plot){
    plot(thresh,accur, type="l" )
    abline(v=cut)
  }
  return(cut)
}
