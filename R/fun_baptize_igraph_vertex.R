#' baptize_igraph_vertex
#'
#' @description Computes the unique name of an igraph vertex used in algo_composite internals.
#' The naming scheme goes like this:
#' For input bits: %algo_id% & NAMESPACE_SEPARATOR & %BIT%, e.g. "node134.i4"
#' For algo: %algo_id% & ".algo", e.g. "node134.algo"
#' For output bits: %algo_id% & NAMESPACE_SEPARATOR & %BIT%, e.g. "node134.o7"
#' This function is mainly used for internal purposes but could be useful for advanced manipulations.
#' This function is trivial but may prove useful in the future to enforce data quality validation rules.
#'
#' @examples print(baptize_igraph_vertex("algo1", "i3"));
#' print(baptize_igraph_vertex("algo2", paste0(INPUT_PREFIX, 1:3)));
#'
#' @param id The node ID character)
#' @param bit The bit (character)
#' @param ... For future usage
#' @return N/A
#' @export
baptize_igraph_vertex <- function(
  id,
  bit,
  ...){

  if(is_missing(id) | is.null(id)){ stop("missing id"); };
  if(is_missing(bit) | is.null(bit)){ bit <- NOBIT_PREFIX; };

  return(paste0(id, NAMESPACE_SEPARATOR, bit));

}
