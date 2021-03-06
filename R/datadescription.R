#'Streamline descriptive analysis of quantitative data matrices
#'@name quantable
#'@docType package
#'@import RColorBrewer
#'@importFrom grDevices dev.off heat.colors
#'@importFrom graphics abline axis image layout lines pairs plot points text
#'@importFrom stats cor.test dist hclust mad median p.adjust qqplot runmed t.test


NULL
# hack to supress _no visible binding for global variable _ warning in R CMD check.
utils::globalVariables(c("ratio"), add = TRUE)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("loading package quantable")
}


#' example data from multigroup analysis
#' @name multigroupFCDATA
#' @docType data
NULL

