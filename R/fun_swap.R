#' swap
#'
#' ...
#'
#' @examples # ...
#'
#' @param composite The composite algorithm where we want to operate the substitution (algo_composite)
#' @param old The old composant algorithm that we want to remove from the composite (algo_base)
#' @param new The new composant algorithm that we want to introduce in the composite (algo_base)
#' @param ... For future usage
#' @return The composite algorithm with the substitution.
#' @export
swap <- function(
  composite,
  old,
  new,
  ...){

  if(!is(composite, "algo_composite")) { stop("The composite parameter does not implement class algo_composite"); };
  if(!is(old, "algo_base")) { stop("The old parameter does not implement class algo_base"); };
  if(!is(new, "algo_base")) { stop("The new parameter does not implement class algo_base"); };
  if(old$get_dim_i() != new$get_dim_i()) { stop("The input dimensions of the old and new algorithms are not identical"); };
  if(old$get_dim_o() != new$get_dim_o()) { stop("The output dimensions of the old and new algorithms are not identical"); };

  # Add the new algo component to the composite.
  composite$set_component(new);

  # Retrieve the internals of the source algo composite
  graph <- composite$get_inner_graph();

  composite_id <- composite$get_algo_id();
  old_id <- old$get_algo_id();
  new_id <- new$get_algo_id();

  # Find the edges linked to the input bits of the old component.
  filter <- E(graph)$target_algo_id == old_id &
    E(graph)$algo_id != old_id;
  edge_list <- E(graph)[filter];
  for(i in 1:length(edge_list)){
    edge <- edge_list[i];
    # Reset that edge for the new algo.
    source_algo_id <- edge$source_algo_id;
    source_algo <- composite$get_component(source_algo_id);
    source_bit <- edge$source_bit;
    target_bit <- edge$target_bit;
    composite$set_inner_edge(
      source_node = source_algo,
      source_bit = source_bit,
      target_node = new,
      target_bit = target_bit);
  }

  # Find the edges linked to the output bits of the old component.
  filter <- E(graph)$source_algo_id == old_id &
    E(graph)$algo_id != old_id;
  edge_list <- E(graph)[filter];
  for(i in 1:length(edge_list)){
    edge <- edge_list[i];
    # Reset that edge for the new algo.
    source_bit <- edge$source_bit;
    target_algo_id <- edge$target_algo_id;
    target_algo <- composite$get_component(target_algo_id);
    target_bit <- edge$target_bit;
    composite$set_inner_edge(
      source_node = new,
      source_bit = source_bit,
      target_node = target_algo,
      target_bit = target_bit);
  }

  # Remove the old component.
  # Assumption: this will automatically delete
  # the remaining igraph vertices and edges.
  remove_component(composite, old);

  return(composite);

}
