require(R6);

#' Algo10 (R6 class)
#'
#' @description The logical algorithm with truth table 01 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_10_graph.png}{Graph of the algorithm}}
#'
#' @usage a <- Algo10$new();
#' a$do_plot();
#' a$do_execute("1");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class Algo10:AlgoComposite:AlgoNode.
#' @export
Algo10 <- R6Class(
  "Algo10",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 1;
      output_dimension <- 1;
      if(is.null(label)){ label <- "10"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Design the algorithm.

      nand1 <- AlgoNAND$new();

      self$set_inner_node(node = nand1);

      self$set_inner_edge(source_node = self, source_bit_id = "i1", target_node = nand1, target_bit_id = "i1");
      self$set_inner_edge(source_node = self, source_bit_id = "i1", target_node = nand1, target_bit_id = "i2");
      self$set_inner_edge(source_node = nand1, source_bit_id = "o1", target_node = self, target_bit_id = "o1");
    }
  )
)
