% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_to_tibble.R
\name{matrix_to_tibble}
\alias{matrix_to_tibble}
\title{matrix or data.frame to tibble (taken from tidyquant)}
\usage{
matrix_to_tibble(x, preserve_row_names = "row.names", ...)
}
\arguments{
\item{x}{a matrix}

\item{preserve_row_names}{give name to rownames column, if NULL discard rownames}

\item{...}{further parameters passed to as_tibble}
}
\description{
matrix or data.frame to tibble (taken from tidyquant)
}
\examples{
x <- matrix(rnorm(20), ncol=4)
rownames(x) <- LETTERS[1:nrow(x)]
matrix_to_tibble(x)
!(is.matrix(x) || is.data.frame(x))

}
