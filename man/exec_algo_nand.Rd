% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_exec_algo_nand.R
\name{exec_algo_nand}
\alias{exec_algo_nand}
\title{Execute the NAND algorithm on a given input.}
\usage{
exec_algo_nand(algo, input, ...)
}
\arguments{
\item{algo}{A NAND algorithm (R6 Class algo_nand)}

\item{input}{The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)}
}
\value{
The corresponding output (same type than input)
}
\description{
...
}
\examples{
# R function style:
execute_algo_nand(algo, input);

# R6 method style:
algo$exec(input);

}
