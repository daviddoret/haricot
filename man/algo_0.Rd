% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_algo_0.R
\docType{data}
\name{algo_0}
\alias{algo_0}
\title{algo_0 (R6 class)}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
algo_0
}
\arguments{
\item{algo_id}{A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)}

\item{label}{A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)}

\item{...}{For future usage.}
}
\value{
An object instance of class algo_0:algo_composite:algo_base.
}
\description{
The 0 constant algorithm. \cr
Viewed from another perspectie: the algorithm whose truth table is: {0}.
}
\section{Graph}{

{\figure{algo_0_graph.png}{Graph of the algorithm}}
}

\examples{
a <- algo_0$new();
a$plot();
a$exec();
a$exec(input = "");
a$exec(input = c(FALSE));
a$exec(input = bnum$new(dim=0));

}
\keyword{datasets}
