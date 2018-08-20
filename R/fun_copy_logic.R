require(igraph);

#' copy_logic, algo_composite$copy_logic_to, algo_composite$copy_logic_from
#'
#' @description Copies the internal logic of a composite algorithm on a different composite algorithm.
#'
#' @examples # R function style:
#' a1 <- algo_xnor$new();
#' a2 <- algo_composite$new(input_dimension = 2, output_dimension = 1);
#' copy_logic(a1, a2);
#' a2$plot();
#'
#' # R6 method style with copy from:
#' a3 <- algo_composite$new(input_dimension = 2, output_dimension = 1);
#' a3$copy_logic_from(a1);
#' a3$plot();
#'
#' # R6 method style with copy to:
#' a4 <- algo_composite$new(input_dimension = 2, output_dimension = 1);
#' a1$copy_logic_to(a4);
#' a4$plot();
#'
#' @param source A composite algorithm (R6 Class algo_composite)
#' @param target A composite algorithm (R6 Class algo_composite)
#' @param ... For future usage
#' @return N/A
#' @export
copy_logic <- function(source, target, ...){

  if(source$get_input_dimension() != target$get_input_dimension()){
    stop("source and target input dimensions are not identical");
  }
  if(source$get_output_dimension() != target$get_output_dimension()){
    stop("source and target output dimensions are not identical");
  }

  if(!is(source, "algo_composite")){
    stop("source is not of class algo_composite");
  };

  if(!is(target, "algo_composite")){
    stop("target is not of class algo_composite");
  };

  # Retrieve the internals of the source algo composite
  target_nodes <- source$get_inner_nodes();
  target_graph <- source$get_inner_graph();

  # Substitute igraph vertices attributes
  vertices_filter <- V(target_graph)$algo_id == source$get_algo_id();
  V(target_graph)[vertices_filter]$algo_id <- target$get_algo_id();
  V(target_graph)[vertices_filter]$name <- baptize_igraph_vertex(target$get_algo_id(), V(target_graph)[vertices_filter]$bit);

  # Substitute igraph edge attributes
  E(target_graph)[E(target_graph)$algo_id == source$get_algo_id()]$algo_id <- target$get_algo_id()
  E(target_graph)[E(target_graph)$source_algo_id == source$get_algo_id()]$source_algo_id <- target$get_algo_id()
  E(target_graph)[E(target_graph)$target_algo_id == source$get_algo_id()]$target_algo_id <- target$get_algo_id()

  # Push the new logic in the target composite algo
  target$set_inner_nodes(target_nodes);
  target$set_inner_graph(target_graph);

}
