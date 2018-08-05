library(R6);

require(R6);

#' AlgoXOR, AlgoTT0110 (R6 class)
#'
#' @description The logical algorithm with truth table 0110 implemented as a NAND-composite.
#' This is also the well-known logical gate XOR.
#'
#' @section Graph:
#' {\figure{algo_tt0110_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 00 \tab 0\cr
#' 10 \tab 1\cr
#' 01 \tab 1\cr
#' 11 \tab 0
#'}
#'
#' @usage a <- AlgoTT0110$new();
#' a$do_plot();
#' a$do_execute("10");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoTT10:AlgoComposite:AlgoNode.
#' @name AlgoTT0110
#' @export
AlgoTT0110 <- R6Class(
  "AlgoTT0110",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT0110"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Apply NAND to the two inputs
      nand1 <- self$add_nand(self, "i1", self, "i2");

      # Invert the first input bit.
      nand2 <- self$add_nand(self, "i1", self, "i1");

      # Inverse the second input bit.
      nand3 <- self$add_nand(self, "i2", self, "i2");

      # Apply NAND to the two inverses.
      nand4 <- self$add_nand(nand2, "o1", nand3, "o1");

      # Apply NAND to the two NANDs
      nand5 <- self$add_nand(nand1, "o1", nand4, "o1");

      # Inverse the result
      nand6 <- self$add_nand(nand5, "o1", nand5, "o1");

      # Pipe the final output.
      self$set_inner_edge(nand6, "o1", self, "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
