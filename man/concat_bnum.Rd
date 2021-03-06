% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_concat_bnum.R
\name{concat_bnum}
\alias{concat_bnum}
\title{Function: concat_bnum}
\usage{
concat_bnum(...)
}
\arguments{
\item{...}{Modular binary numbers (bnum)}
}
\value{
The result of the concatenation
}
\description{
Concatenates modular binary numbers.
}
\section{References}{

\itemize{
\item{Concatenation of modular binary numbers (\link{concatenation_modular_binary_numbers})}
}
}

\examples{
Function style:
n1 <- bnum$new("0101");
n2 <- bnum("11000");
n3 <- concat_bnum(n1, n2);
print(n3);

R6 method style:
n4 <- n1$concat_with(n2);
print(n4);

Infix operator style:
n5 <- n1 \%concat\% n2;
print(n5);

}
\concept{concatenations}
