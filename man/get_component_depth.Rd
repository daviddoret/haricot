% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_get_component_depth.R
\name{get_component_depth}
\alias{get_component_depth}
\title{get_component_depth}
\usage{
get_component_depth(composite, component, ...)
}
\arguments{
\item{composite}{The composite within which we want to measure the depth of a component (R6 class algo_composite).}

\item{component}{The component algorithm that must be part of the composite and whose depth we would like to know (R6 class algo_base | character vector of the component ID).}
}
\value{
The depth of the component (integer).
}
\description{
Returns the depth of a component in a composite algorithm. \cr
\cr
\strong{Definition} \cr
The depth of a component is equal to: \cr
\itemize{
\item{1 if the component is a constant (ie it has no predecessors),}
\item{2 if the component is only directly linked to its parent composite,}
\item{the maximal depth of its predecessors + 1 otherwise.}
} \cr
\strong{Formula} \cr
\deqn{
depth(x)=\begin{cases}
1, & \text{if}\ |N^{-}(x)| = 0 \\
2, & \text{if}\ \forall y \in N^{-}(x), y = composite(x) \\
max(depth(N^{-}(x)))+1, & \text{otherwise}
\end{cases}
}
\strong{Note} \cr
In the haricot package, when we plot a composite algorithm,
we plot the input bits and output bits as vertices.
This representation is interesting because it shows the bit-level
interfaces between components.
But the depth concept does not consider input bits and output bits,
and only consider components as single units (the algorithm and its surrounding bits).
The reason for this is to account for the algorithmic complexity or size, rather than the detailed bit mechanics.
}
\examples{
...

}
