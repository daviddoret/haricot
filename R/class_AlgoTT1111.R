library(R6);

require(R6);

#' AlgoTT1111 (R6 class)
#'
#' @description The logical algorithm with truth table 1111 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_tt1111_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 00 \tab 1\cr
#' 10 \tab 1\cr
#' 01 \tab 1\cr
#' 11 \tab 1
#'}
#'
#' @examples a <- AlgoTT1111$new();
#' a$do_plot();
#' a$do_execute("10");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoTT10:AlgoComposite:AlgoNode.
#' @export
AlgoTT1111 <- R6Class(
  "AlgoTT1111",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT1111"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Inverse input bit 1.
      nand1 <- self$add_nand(self, "i1", self, "i1");

      # Apply NAND to input bit 1 and its inverse.
      nand2 <- self$add_nand(self, "i1", nand1, "o1");

      # Pipe the final output.
      self$set_inner_edge(nand2, "o1", self, "o1");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
