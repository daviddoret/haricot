% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_algo_1100.R
\docType{data}
\name{algo_1100}
\alias{algo_1100}
\title{algo_1100 (R6 class)}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
algo_1100
}
\arguments{
\item{algo_id}{A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)}

\item{label}{A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)}

\item{...}{For future usage.}
}
\value{
An object instance of class algo_10:algo_composite:algo_base.
}
\description{
The logical algorithm with truth table 1100 implemented as a NAND-composite.
}
\section{Graph}{

{\figure{algo_1100_graph.png}{Graph of the algorithm}}
}

\section{Truth table}{

\tabular{ll}{
\strong{input} \tab \strong{output}\cr
00 \tab 1\cr
10 \tab 1\cr
01 \tab 0\cr
11 \tab 0
}
}

\examples{
a <- algo_1100$new();
a$plot();
a$exec("10");

}
\keyword{datasets}
