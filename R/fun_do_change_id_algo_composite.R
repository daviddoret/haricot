#' do_change_id_algo_composite, algo_composite$do_change_id
#'
#' @description Modifies the unique ID of a composite algorithm node.
#'
#' @examples # R function style:
#' a1 <- algo_xnor$new();
#' print(a1$get_algo_id());
#' do_change_id_algo_composite(a1, "a_stupid_id");
#' print(a1$get_algo_id());
#'
#' # R6 method style:
#' a2 <- algo_xnor$new();
#' print(a2$get_algo_id());
#' a2$do_change_id("another_stupid_id");
#' print(a2$get_algo_id());
#'
#' @param algo A composite algorithm (R6 Class algo_composite)
#' @param id A unique identifier (character)
#' @param ... For future usage
#' @return N/A
#' @export
do_change_id_algo_composite <- function(algo, id, ...){

  # TODO: Check class of algo and id

  g1 <- algo$get_inner_graph();
  legacy_id <- algo$get_algo_id();

  # Substitute algo_id in inner graph
  stop("COMPLETE IMPLEMENTATION");
  #E(g1)[E(g1)$source_algo_id == legacy_id]$source_algo_id <- id;
  #E(g1)[E(g1)$target_algo_id == legacy_id]$target_algo_id <- id;
  #E(g1)[]
  stop("substitute edge$target_algo_id");
  stop("substitute edge$name");

  # Push the new logic in the target composite algo
  target$set_inner_nodes(target_inner_nodes);
  target$set_inner_graph(target_inner_graph);

}
