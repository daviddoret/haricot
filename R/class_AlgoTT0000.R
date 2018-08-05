library(R6);

require(R6);

#' AlgoTT0000 (R6 class)
#'
#' @description The logical algorithm with truth table 0000 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_tt0000_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 00 \tab 0\cr
#' 10 \tab 0\cr
#' 01 \tab 0\cr
#' 11 \tab 0
#'}
#'
#' @usage a <- AlgoTT0000$new();
#' a$do_plot();
#' a$do_execute("10");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoTT10:AlgoComposite:AlgoNode.
#' @export
AlgoTT0000 <- R6Class(
  "AlgoTT0000",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT0000"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Invert the first input bit.
      nand1 <- self$add_nand(source_1_node = self, source_1_bit = "i1", source_2_node = self, source_2_bit = "i1");

      # NAND the first input bit with its inverse.
      # The output is unconditionnaly TRUE.
      nand2 <- self$add_nand(source_1_node = self, source_1_bit = "i1", source_2_node = nand1, source_2_bit = "o1");

      # Invert output to get an unconditional FALSE.
      nand3 <- self$add_nand(source_1_node = nand2, source_1_bit = "o1", source_2_node = nand2, source_2_bit = "o1");

      # Pipe the final output.
      self$set_inner_edge(source_node = nand3, source_bit = "o1", target_node = self, target_bit = "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
