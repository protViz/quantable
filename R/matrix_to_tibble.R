#' matrix or data.frame to tibble (taken from tidyquant)
#' 
#' @param x a matrix
#' @param preserve_row_names give name to rownames column, if NULL discard rownames
#' @param ... further parameters passed to as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble as_tibble
#' @export
#' @examples 
#' x <- matrix(rnorm(20), ncol=4)
#' rownames(x) <- LETTERS[1:nrow(x)]
#' matrix_to_tibble(x)
#' !(is.matrix(x) || is.data.frame(x))
#' 
matrix_to_tibble <- function(x, preserve_row_names = "row.names",...)
{
  if (!(is.matrix(x) || is.data.frame(x))) stop("Error: `x` is not a matrix or data.frame object.")
  if (!is.null(preserve_row_names)) {
    row.names <- rownames(x)
    if (!is.null(row.names)  ) {
      #&& !identical(row.names, 1:nrow(x) %>% as.character())
      dplyr::bind_cols(
        tibble::tibble(!! preserve_row_names := row.names),
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

