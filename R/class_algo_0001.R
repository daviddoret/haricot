library(R6);

require(R6);

#' algo_nand, algo_0001 (R6 class)
#'
#' @description The logical algorithm with truth table 0001 implemented as a NAND-composite.
#' This is also the well-known logical gate AND.
#'
#' @section Graph:
#' {\figure{algo_0001_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 00 \tab 0\cr
#' 10 \tab 0\cr
#' 01 \tab 0\cr
#' 11 \tab 1
#'}
#'
#' @examples a <- algo_0001$new();
#' a$plot();
#' a$exec("10");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_10:algo_composite:algo_base.
#' @name algo_0001
#' @export
algo_0001 <- R6Class(
  "algo_0001",
  inherit = algo_composite,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT0001"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        algo_id = algo_id,
        label = label);
      #...);

      # Apply NAND to the two input bits.
      nand1 <- self$add_nand(source_1_node = self, source_1_bit = "i1", source_2_node = self, source_2_bit = "i2");

      # Inverse the result.
      nand2 <- self$add_nand(source_1_node = nand1, source_1_bit = "o1", source_2_node = nand1, source_2_bit = "o1");

      # Pipe the final output.
      self$set_inner_edge(source_node = nand2, source_bit = "o1", target_node = self, target_bit = "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
