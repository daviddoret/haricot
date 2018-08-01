require(igraph);
#' Convert an object of type CompositeAlgoInnerNode to an igraph object.
#'
#' @description The igraph network will be composed of:
#' * One node per input bit,
#' * One node representing the algorithm,
#' * One node per output bit,
#' * Directed edges from the input bits to the algorithm node,
#' * Directed edges from the algorithm node to the output bits.
#'
#' @usage g <- convert_CompositeAlgoInnerNode_to_igraph(node, ...);
#'
#' @param node The node that we want to graph (CompositeAlgoInnerNode)
#' @param ... For future usage.
#' @return A directed network graph representation of the node (igraph).
#' @export
convert_CompositeAlgoInnerNode_to_igraph <- function(node, ...){

  g <- make_empty_graph(directed = TRUE) %>%
    add_vertices(
      nv = node$get_input_dimension(),
      bit_id = paste0("i", 1:node$get_input_dimension()),
      label = paste0("i", 1:node$get_input_dimension()),
      name = paste0(node$get_node_id(), ".", paste0("i", 1:node$get_input_dimension())),
      node_id = node$get_node_id(),
      type = "inputbit_vertex") %>%
    add_vertices(
      nv = 1,
      bit_id = NA,
      label = node$get_label(),
      name = paste0(node$get_node_id(), ".", "algo"),
      node_id = node$get_node_id(),
      type = "algo_vertex") %>%
    add_vertices(
      nv = node$get_output_dimension(),
      bit_id = paste0("o", 1:node$get_output_dimension()),
      label = paste0("o", 1:node$get_output_dimension()),
      name = paste0(node$get_node_id(),".",paste0("o", 1:node$get_output_dimension())),
      node_id = node$get_node_id(),
      type = "outputbit_vertex") %>%
    add_edges(
      c(rbind(
          1:node$get_input_dimension(),
          rep(node$get_input_dimension() + 1, node$get_input_dimension())
          )),
      node_id = node$get_node_id(),
      type = "input_algo_edge") %>%
    add_edges(
      c(rbind(
          rep(node$get_input_dimension() + 1, node$get_output_dimension()),
          node$get_input_dimension() + 1 + 1:node$get_output_dimension()
          )),
      node_id = node$get_node_id(),
      type = "algo_output_edge");
  return(g);

}

