#' Matrix to tibble (taken from tidyquant)
#' 
#' @param x a matrix
#' @param preserve_row_names
#'  
#' @export
#' 
matrix_to_tibble <- function(x, preserve_row_names = TRUE)
{
  if (!is.matrix(x)) stop("Error: `x` is not a matrix object.")
  if (preserve_row_names == TRUE) {
    row.names <- rownames(x)
    # Detect if row.names exist beyond sequential 1:nrow(x) or null value
    if (!identical(row.names, 1:nrow(x) %>% as.character()) &&
        !is.null(row.names)) {
      dplyr::bind_cols(
        tibble::tibble(row.names),
        tibble::as_tibble(x, ...)
      )
      
    } else {
      
      warning(paste0("Warning: No row names to preserve. ",
                     "Object otherwise converted to tibble successfully."))
      tibble::as_tibble(x, ...)
    }
    
  } else {
    
    tibble::as_tibble(x, ...)
    
  }
}