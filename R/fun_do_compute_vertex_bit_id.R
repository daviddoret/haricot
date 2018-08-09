#' do_compute_vertex_bit
#'
#' @description Computes the "bit" attribute of igraph graphs in CompositeNode.
#' The naming scheme goes like this:
#' For input bit vertices: "i1", "i2", "i3", ...
#' For algo vertices: "" (empty string)
#' For output bit vertices: "o1", "o2", "o3", ...
#' This function is mainly used for internal purposes but could be useful for advanced manipulations.
#' This function is trivial but may prove useful in the future to enforce data quality validation rules.
#'
#' @examples print(do_compute_vertex_bit("i", 5));
#' print(do_compute_vertex_bit("o", 1:4));
#'
#' @param type The bit type: "i" or "o" (character)
#' @param number The bit number (integer)
#' @param ... For future usage
#' @return The bit name
#' @export
do_compute_vertex_bit <- function(
  type,
  number,
  ...){

  if(is_missing(type) | is.null(type)){ stop("missing type"); };
  if(type != "i" & type != "n" & type != "o"){ stop("invalid type"); };
  if(is_missing(number) | is.null(number)){ stop("missing number"); };

  return(paste0(type, number));

}
