library(R6);

require(R6);

#' algo_0101 (R6 class)
#'
#' @description The logical algorithm with truth table 0101 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_tt0101_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 00 \tab 0\cr
#' 10 \tab 1\cr
#' 01 \tab 0\cr
#' 11 \tab 1
#'}
#'
#' @examples a <- algo_0101$new();
#' a$do_plot();
#' a$do_execute("10");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_10:algo_composite:algo_base.
#' @export
algo_0101 <- R6Class(
  "algo_0101",
  inherit = algo_composite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT0101"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Inverse input bit 1.
      nand1 <- self$add_nand(self, "i1", self, "i1");

      # Apply NAND to the two inputs.
      nand2 <- self$add_nand(self, "i1", self, "i2");

      # Apply NAND to the above two results.
      nand3 <- self$add_nand(nand1, "o1", nand2, "o1");

      # Pipe the final output.
      self$set_inner_edge(nand3, "o1", self, "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
