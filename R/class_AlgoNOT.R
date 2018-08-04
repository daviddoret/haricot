require(R6);

#' AlgoNOT
#'
#' @description The NOT logical algorithm implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_not_graph.png}{Graph of the algorithm}}
#'
#' @usage a <- AlgoNOT$new();
#' a$do_plot();
#' a$do_execute("1");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoNOT:AlgoComposite:AlgoNode.
#' @export
AlgoNOT <- R6Class(
  "AlgoNOT",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 1;
      output_dimension <- 1;
      if(is.null(label)){ label <- "NOT"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
        #label = label,
        #...);

      # Design the algorithm.

      nand1 <- AlgoNAND$new();

      self$set_inner_node(node = nand1);

      self$set_inner_edge(
        source_node_id = self$get_node_id(),
        source_bit_id = "i1",
        target_node_id = nand1$get_node_id(),
        target_bit_id = "i1");

      self$set_inner_edge(
        source_node_id = self$get_node_id(),
        source_bit_id = "i1",
        target_node_id = nand1$get_node_id(),
        target_bit_id = "i2");

      self$set_inner_edge(
        source_node_id = nand1$get_node_id(),
        source_bit_id = "o1",
        target_node_id = self$get_node_id(),
        target_bit_id = "o1");
    }
  )
)
