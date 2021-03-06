% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_exec_algo_1.R
\name{exec_algo_1}
\alias{exec_algo_1}
\title{Execute the 1 constant algorithm on no or a null input.}
\usage{
exec_algo_1(algo, input = NULL, ...)
}
\arguments{
\item{algo}{A 1 constant algorithm (R6 Class algo_1)}

\item{input}{(conditional) The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)}
}
\value{
The corresponding output (same type than input, default type: logical vector)
}
\description{
...
}
\examples{
# R function style:
exec_algo_1(algo, input);

# R6 method style:
a <- algo_1$new();
a$exec(input);

}
