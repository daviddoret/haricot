#' Function: set_graph_edge
#'
#' @description Set a graph edge in a composite algorithm.
#'
#' @examples ...
#'
#' @param composite The algorithm composite whose inner graph we want to manipulate (algo_composite).
#' @param source_node The source algorithm of the directed edge (algo_base or algo_id as character).
#' @param source_bit The source bit of the directed edge (bit identifier as character).
#' @param target_node The taget algorithm of the directed edge (algo_base or algo_id as character).
#' @param target_bit The target bit of the directed edge (bit identifier as character).
#' @param ... Complementary parameters.
#' @return The composite algorithm with the updated graph (algo_composite).
#' @export
set_graph_edge = function(
  composite,
  source_node,
  source_bit,
  target_node,
  target_bit,
  ...){

  if(!is(composite, "algo_composite")){
    stop("Composite is not of class algo_composite");
  }

  if(is_missing(source_node)){
    # If the component is not specified,
    # we assume the intention is to work directly
    # on the input and output bits of the current node.
    source_node <- composite;
  }
  if(is_missing(target_node)){
    # If the component is not specified,
    # we assume the intention is to work directly
    # on the input and output bits of the current node.
    target_node <- composite;
  }
  source_algo_id <- source_node$get_algo_id();
  target_algo_id <- target_node$get_algo_id();

  source_name <- paste0(source_algo_id, NAMESPACE_SEPARATOR, source_bit);
  target_name <- paste0(target_algo_id, NAMESPACE_SEPARATOR, target_bit);

  flog.debug(fun = "set_graph_edge", composite = composite$get_label(), source_name = source_name, target_name = target_name, ...);

  g <- composite$get_dag();

  # Determine the type of edge from node classes.
  source_vertex <- V(g)[V(g)$name == source_name];
  target_vertex <- V(g)[V(g)$name == target_name];
  type <- paste0(source_vertex$type, "_", target_vertex$type);
  # TODO: Check that we only manipulate manipulatable vertexes,
  # i.e. only InputBits and OutputBits.

  # InputBit and OutputBit nodes can only have a single inbound edge.
  # Delete the existing edges to guarantee graph consistency.
  g <- delete_edges(graph = g, edges = E(g)[to(target_name)]);

  color <- switch(
    type,
    "inputbit_inputbit" = "#00994c",
    "outputbit_outputbit" = "#004c99",
    "inputbit_outputbit" = "#00994c",
    "outputbit_inputbit" = "#004c99");

  new_edges <- c(source_name, target_name);
  #print(paste0("new_edges:", new_edges));
  #print(paste0("type:", type));
  #print(paste0("color:", color));

  g <- add_edges(
    graph = g,
    edges = new_edges,
    algo_id = composite$get_algo_id(),
    source_algo_id = source_algo_id,
    source_bit = source_bit,
    target_algo_id = target_algo_id,
    target_bit = target_bit,
    arrow.size = .1,
    arrow.width = 2,
    color = color,
    lty = "solid",
    type = type);
  # TODO: Add attributes for style, etc.

  composite$set_dag(g, ...);

  return(composite);
}
