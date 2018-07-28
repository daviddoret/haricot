% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_int_to_logical_vector.R
\name{convert_int_to_logical_vector}
\alias{convert_int_to_logical_vector}
\title{Convert a native R integer value into a logical vector representation.}
\usage{
# R function style:
convert_int_to_logical_vector(i, size);
}
\arguments{
\item{i}{An integer (integer)}

\item{size}{The size (length) of the logical vector (integer)}
}
\value{
The logical vector (logical)
}
\description{
In this package, we often use logical vectors to represent modulo integer values.
In this context, we use a binary representation where the least significant bit is on the left (at vector index position 1).
This function takes a native R integer value and converts it into such a logical vector.
}