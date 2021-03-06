#' Composite algorithm
#'
#' In the context of this package, a composite algorithm is defined as: \cr
#' A set of component algorithms,
#' A directed acyclic graph,
#' The following constraints:
#' the dag vertices are of the following types
#' a set of 0-n composite input bits vertices, noted \eqn{b_{0}, b_{1}}
#' a set of 1-n composite output bits vertices
#' a set of 0-n composant algorithm vertices
#' a set of 0-n composant input bit vertices
#' a set of 0-n composant output bit vertices
#' composite input bit vertices can only be linked to:
#' - composite algorithm input bit vertices
#' - composite output bit vertices
#' composite output bit vertices cannot be linked, they are the final vertices
#' composant algorithm vertices can only be linked to:
#' - composant output bit vertices
#' composant input bit vertices can only be linked to:
#' - composant algorithm vertices
#' composnat output bit vertices can only be linked to:
#' - composant input bit vertices
#' - composite output bit vertices
#'
#' @section References:
#' \itemize{
#' \item{\code{\link{algo_composite}} R6 class}
#' }
#'
#' @name composite_algorithm
NULL;
