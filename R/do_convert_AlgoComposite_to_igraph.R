require(R6);
require(rlang);
require(igraph);

#' Convert an object of type AlgoComposite to an igraph object.
#'
#' @description The igraph network will be composed of:
#' * One node per input bit,
#' * One node per algorithm,
#' * One node per output bit,
#' * Directed edges from the input bits to the algorithm node,
#' * Directed edges from the algorithm node to the output bits,
#' * Directed edges between sub-algorithms.
#'
#' @examples g <- convert_AlgoComposite_to_igraph(node, ...);
#'
#' @param node The node that we want to graph (AlgoComposite)
#' @param ... For future usage.
#' @return A directed network graph representation of the node (igraph).
#' @export
do_convert_AlgoComposite_to_igraph <- function(node, ...){

  # AlgoComposite manage its internal state with an igraph...
  g <- node$get_inner_graph();
  return(g);

}

