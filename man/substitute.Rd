% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_substitute.R
\name{substitute}
\alias{substitute}
\title{substitute}
\usage{
substitute(compi, orig, subst, ...)
}
\arguments{
\item{compi}{The composite algorithm inside which we want to perform the substitution (R6 Class algo_composite)}

\item{orig}{The original component algorithm that we want to extract from the composite (R6 Class algo_base)}

\item{subst}{The new component algorithm that we want to inject in the composite (R6 Class algo_base)}

\item{...}{For future usage}
}
\value{
N/A
}
\description{
Substitute a component algorithm with another in a composite algorithm. \cr
The two component algorithms must be of equivalent dimensions.
}
\examples{
# ...

}
