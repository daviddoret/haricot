% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_force_matrix.R
\name{force_matrix}
\alias{force_matrix}
\title{Function: force_matrix}
\usage{
force_matrix(m = NULL, vector_direction = NULL, ...)
}
\arguments{
\item{m}{A (perhaps already implicitely converted) logical matrix (logical matrix, vector or atomic logical value).}

\item{vector_direction}{VECTOR_DIRECTION_HORIZONTAL or VECTOR_DIRECTION_VERTICAL, depending on how we want vectors to be interpreted.}
}
\value{
A strict matrix type of any size.
}
\description{
The inner logic of truth table algorithms is composed of a logical matrix.
I observe that R implicitely converts 1 dimensional matrixes to vectors, etc.
This is not the behavior I want because I need to be able to work with the matrix independently of its size.
This function forces the creation of a matrix, even if it is 1-by-1, 1-by-x or x-by-1.
}
\examples{
...

}
