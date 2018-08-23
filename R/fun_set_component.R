#' set_component
#'
#' @description Set a component algorithm in a composite algorithm.
#'
#' @examples ...
#'
#' @param composite The composite algorithm we want to manipulate (algo_composite).
#' @param node The component algorithm we want to inject (algo_base).
#' @return The modified composite algorithm (algo_composite).
#' @export
set_component = function(
  composite,
  node,
  ...){

  if(!is(composite, "algo_composite")){
    stop("composite is not of class algo_composite");
  };
  if(!is(node, "algo_base")){
    stop("component is not of class algo_base");
  };

  # TODO: If the node exists already, clean the igraph properly.
  # Do this by checking node_id unicity.

  # Retrieve the node unique ID
  algo_id <- node$get_algo_id();

  composite_label <- composite$get_label();
  component_label <- node$get_label();
  log(fun = "set_component", composite = composite_label, node = component_label, ...);

  # Store the sub-algorithm node in the private list.
  component_list <- composite$get_components();
  component_list[[algo_id]] <- node;
  composite$set_components(component_list);

  # Merge the current graph with the new one.
  component_graph <- node$convert_to_igraph();
  composite_graph <- composite$get_inner_graph();
  merged_graph <- composite_graph %du% component_graph
  composite$set_inner_graph(merged_graph);

  # Of course, at this point the new sub-graph will be disconnected.

  # Chaining.
  return(composite);
}
