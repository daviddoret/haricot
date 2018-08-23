#' remove_component
#'
#' Remove a component algorithm from a composite algorithm.
#'
#' @examples # ...
#'
#' @param composite The composite algorithm (algo_composite)
#' @param old The component algorithm (algo_base)
#' @param ... For future usage
#' @return The composite algorithm with the component remove (algo_composite)
#' @export
remove_component <- function(
  composite,
  component,
  ...){

  if(!is(composite, "algo_composite")) { stop("The composite parameter does not implement class algo_composite"); };
  if(!is(component, "algo_base")) { stop("The component parameter does not implement class algo_base"); };

  id <- component$get_algo_id();
  graph <- composite$get_dag();

  # Remove the igraph vertices linked to this algorithm component
  vertices_filter <- V(graph)$algo_id == id;
  graph <- delete_vertices(graph, vertices_filter);

  # Remove igraph edges
  # My expectation from igraph is the deleting a vertex
  # automatically deletes the related edges.
  # But this point was not clearly stated in the doc.

  composite$set_dag(graph);

  components <- list.remove(composite$get_components(), id);
  composite$set_components(components);

  return(composite);

}
