#' do_change_id_AlgoComposite, AlgoComposite$do_change_id
#'
#' @description Modifies the unique ID of a composite algorithm node.
#'
#' @examples # R function style:
#' a1 <- AlgoXNOR$new();
#' print(a1$get_node_id());
#' do_change_id_AlgoComposite(a1, "a_stupid_id");
#' print(a1$get_node_id());
#'
#' # R6 method style:
#' a2 <- AlgoXNOR$new();
#' print(a2$get_node_id());
#' a2$do_change_id("another_stupid_id");
#' print(a2$get_node_id());
#'
#' @param algo A composite algorithm (R6 Class AlgoComposite)
#' @param id A unique identifier (character)
#' @param ... For future usage
#' @return N/A
#' @export
do_change_id_AlgoComposite <- function(algo, id, ...){

  # TODO: Check class of algo and id

  g1 <- algo$get_inner_graph();
  legacy_id <- algo$get_node_id();

  # Substitute node_id in inner graph
  stop("COMPLETE IMPLEMENTATION");
  #E(g1)[E(g1)$source_node_id == legacy_id]$source_node_id <- id;
  #E(g1)[E(g1)$target_node_id == legacy_id]$target_node_id <- id;
  #E(g1)[]
  stop("substitute edge$target_node_id");
  stop("substitute edge$name");

  # Push the new logic in the target composite algo
  target$set_inner_nodes(target_inner_nodes);
  target$set_inner_graph(target_inner_graph);

}
