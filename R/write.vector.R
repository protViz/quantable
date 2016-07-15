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