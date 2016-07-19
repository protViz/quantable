#' write vectors as single column table
#' (usefull for exporting i.e. protein id's)
#' uses write.table
#' @param x vector to write
#' @param file file to write to
#' @export
#' 
write.vector <- function(x,file){
  utils::write.table(x,file=file, quote=FALSE,row.names = FALSE,sep="\t",col.names = FALSE)  
}
#' write table in tab delimited no quotes no row.names
#' (usefull for exporting i.e. list of foldchanges)
#' uses write.table 
#' TODO : validate if not redundant with other write functions
#' @param x vector to write
#' @param file file to write to
#' @export
#' 
write.tab <- function(x, file){
  utils::write.table(x,file=file, quote=FALSE,row.names = FALSE,sep="\t",col.names = TRUE)  
}