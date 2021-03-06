% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_plot_bset.R
\name{plot_bset}
\alias{plot_bset}
\title{plot_bset}
\usage{
plot_bset(d, ...)
}
\arguments{
\item{d}{The binary set to be plotted (R6 class bset).}

\item{...}{For future usage.}
}
\value{
A plotted binary number.
}
\description{
Generate a visualization of a binary set,
following the general look and feel of the haricot package.
}
\examples{
# R function style:
d <- bset$new(3);
plot_bset(d);

# R6 class method style:
d$plot();

}
