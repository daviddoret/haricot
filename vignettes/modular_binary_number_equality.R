#' @section Theory:
#'
#' \subsection{Equality of Modular Binary Numbers}{
#' In the context of this package, two modular binary numbers are equal if and only if they have identical dimensions and all their corresponding bits are equal. \cr
#' Formally: \cr
#' \deqn{\forall i \in \mathbb{N}^+ , (a,b) \in \mathbb{B}_{i} : a=b \iff |a| = |b|, \forall j \in [1,...,i] , a_{i} = b_{i}}
#' }
#' A consequence of this definition of equality is that 2 modular binary numbers may be mapped to the same natural number and not be considered equal.\cr
#' For instance: \cr
#'
#'
#' @name bnum_equality
NULL;
