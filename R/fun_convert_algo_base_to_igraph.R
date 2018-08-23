require(R6);
require(rlang);
require(igraph);

#' Convert an object of class algo_base to an object of class igraph.
#'
#' @description The igraph network will be composed of:
#' * One node per input bit,
#' * One node representing the algorithm,
#' * One node per output bit,
#' * Directed edges from the input bits to the algorithm node,
#' * Directed edges from the algorithm node to the output bits.
#'
#' @examples g <- convert_algo_base_to_igraph(node, ...);
#'
#' @param node The node that we want to graph (algo_base)
#' @param ... For future usage.
#' @return A directed network graph representation of the node (igraph).
#' @export
convert_algo_base_to_igraph <- function(node, ...){

  g <- make_empty_graph(directed = TRUE);
  g <- add_vertices(
    graph = g,
    nv = 1,
    bit = baptize_algo_bit(NOBIT_PREFIX),
    color = "#eeeeee",
    label = node$get_label(),
    name = paste0(node$get_algo_id(), NAMESPACE_SEPARATOR, NOBIT_PREFIX),
    algo_id = node$get_algo_id(),
    push_execution_value = list(), # A vector of pushed execution values.
    shape = "circle",
    size = 20,
    type = "algo");
  if(node$get_dim_i() > 0){
    g <- add_vertices(
      graph = g,
      nv = node$get_dim_i(),
      bit = baptize_algo_bit(INPUT_PREFIX, 1:node$get_dim_i()),
      color = "#ccffe5",
      label = paste0(INPUT_PREFIX, 1:node$get_dim_i()),
      name = paste0(node$get_algo_id(), NAMESPACE_SEPARATOR, paste0(INPUT_PREFIX, 1:node$get_dim_i())),
      algo_id = node$get_algo_id(),
      push_execution_value = list(), # A vector of pushed execution values.
      shape = "circle",
      size = 10,
      type = "inputbit");
    g <- add_edges(
      graph = g,
      c(rbind(
        1 + (1:node$get_dim_i()),
        1
      )),
      algo_id = node$get_algo_id(),
      source_algo_id = node$get_algo_id(),
      source_bit = baptize_algo_bit(INPUT_PREFIX, 1:node$get_dim_i()),
      target_algo_id = node$get_algo_id(),
      target_bit = baptize_algo_bit(NOBIT_PREFIX),
      arrow.size = .1,
      arrow.width = 1,
      #weight = .9,
      color = "#00994c",
      lty = "solid",
      type = "input_algo");
  };
  if(node$get_dim_o() > 0){
    g <- add_vertices(
        graph = g,
        nv = node$get_dim_o(),
        bit = baptize_algo_bit(OUTPUT_PREFIX, 1:node$get_dim_o()),
        color = "#cce5ff",
        label = paste0(OUTPUT_PREFIX, 1:node$get_dim_o()),
        name = paste0(node$get_algo_id(),NAMESPACE_SEPARATOR,paste0(OUTPUT_PREFIX, 1:node$get_dim_o())),
        algo_id = node$get_algo_id(),
        push_execution_value = list(), # A vector of pushed execution values.
        shape = "circle",
        size = 10,
        type = "outputbit");
    g <- add_edges(
        graph = g,
        c(rbind(
          1,
          1 + node$get_dim_i() + (1:node$get_dim_o())
        )),
        algo_id = node$get_algo_id(),
        source_algo_id = node$get_algo_id(),
        source_bit = baptize_algo_bit(NOBIT_PREFIX),
        target_algo_id = node$get_algo_id(),
        target_bit = baptize_algo_bit(OUTPUT_PREFIX, 1: node$get_dim_o()),
        arrow.size = .1,
        arrow.width = 1,
        color = "#004c99",
        lty = "solid",
        type = "algo_output");
  };
  return(g);

}

