#' do_compute_vertex_name
#'
#' @description Computes the unique name of an igraph vertex used in AlgoComposite internals.
#' The naming scheme goes like this:
#' For input bits: %NODE_ID% & "." & %BIT%, e.g. "node134.i4"
#' For algo: %NODE_ID% & ".algo", e.g. "node134.algo"
#' For output bits: %NODE_ID% & "." & %BIT%, e.g. "node134.o7"
#' This function is mainly used for internal purposes but could be useful for advanced manipulations.
#' This function is trivial but may prove useful in the future to enforce data quality validation rules.
#'
#' @examples print(do_compute_vertex_name("algo1", "i3"));
#' print(do_compute_vertex_name("algo2", paste0("i", 1:3)));
#'
#' @param id The node ID character)
#' @param bit The bit (character)
#' @param ... For future usage
#' @return N/A
#' @export
do_compute_vertex_name <- function(
  id,
  bit,
  ...){

  if(is_missing(id) | is.null(id)){ stop("missing id"); };
  if(is_missing(bit) | is.null(bit)){ bit <- "algo"; };

  return(paste0(id, ".", bit));

}
