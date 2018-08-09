require(R6);
require(rlang);
require(igraph);

#' Convert an object of type algo_base to an igraph object.
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

  g <- make_empty_graph(directed = TRUE) %>%
    add_vertices(
      nv = node$get_input_dimension(),
      bit_id = paste0("i", 1:node$get_input_dimension()),
      color = "#ccffe5",
      label = paste0("i", 1:node$get_input_dimension()),
      name = paste0(node$get_algo_id(), ".", paste0("i", 1:node$get_input_dimension())),
      algo_id = node$get_algo_id(),
      push_execution_value = list(), # A vector of pushed execution values.
      shape = "circle",
      size = 10,
      type = "inputbit") %>%
    add_vertices(
      nv = 1,
      bit_id = NA,
      color = "#eeeeee",
      label = node$get_label(),
      name = paste0(node$get_algo_id(), ".", "algo"),
      algo_id = node$get_algo_id(),
      push_execution_value = list(), # A vector of pushed execution values.
      shape = "circle",
      size = 20,
      type = "algo") %>%
    add_vertices(
      nv = node$get_output_dimension(),
      bit_id = paste0("o", 1:node$get_output_dimension()),
      color = "#cce5ff",
      label = paste0("o", 1:node$get_output_dimension()),
      name = paste0(node$get_algo_id(),".",paste0("o", 1:node$get_output_dimension())),
      algo_id = node$get_algo_id(),
      push_execution_value = list(), # A vector of pushed execution values.
      shape = "circle",
      size = 10,
      type = "outputbit") %>%
    add_edges(
      c(rbind(
          1:node$get_input_dimension(),
          rep(node$get_input_dimension() + 1, node$get_input_dimension())
          )),
      algo_id = node$get_algo_id(),
      source_algo_id = node$get_algo_id(),
      # TODO: ADD source_bit
      target_algo_id = node$get_algo_id(),
      # TODO: ADD target_bit
      arrow.size = .1,
      arrow.width = 1,
      #weight = .9,
      color = "#00994c",
      lty = "solid",
      type = "input_algo") %>%
    add_edges(
      c(rbind(
          rep(node$get_input_dimension() + 1, node$get_output_dimension()),
          node$get_input_dimension() + 1 + 1:node$get_output_dimension()
          )),
      algo_id = node$get_algo_id(),
      source_algo_id = node$get_algo_id(),
      # TODO: ADD source_bit
      target_algo_id = node$get_algo_id(),
      # TODO: ADD target_bit
      arrow.size = .1,
      arrow.width = 1,
      color = "#004c99",
      lty = "solid",
      type = "algo_output");
  return(g);

}

