require(R6);

#' algo_xnor, algo_1001 (R6 class)
#'
#' @description The logical algorithm with truth table 1001 implemented as a NAND-composite.
#' This is also the well-known logical gate XNOR.
#'
#' @section Graph:
#' {\figure{algo_1001_graph.png}{Graph of the algorithm}}
#'
#' @examples a <- algo_1001$new();
#' a$plot();
#' a$do_execute("10");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_10:algo_composite:algo_base.
#' @name algo_1001
#' @export
algo_1001 <- R6Class(
  "algo_1001",
  inherit = algo_composite,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT1001"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        algo_id = algo_id); #,
      #label = label,
      #...);

      # Design the algorithm.
      # Apply NAND to the two inputs
      nand1 <- self$add_nand(source_1_node = self, source_1_bit = "i1", source_2_node = self, source_2_bit = "i2");

      # Invert the first input bit.
      nand2 <- self$add_nand(source_1_node = self, source_1_bit = "i1", source_2_node = self, source_2_bit = "i1");

      # Inverse the second input bit.
      nand3 <- self$add_nand(source_1_node = self, source_1_bit = "i2", source_2_node = self, source_2_bit = "i2");

      # Apply NAND to the two inverses.
      nand4 <- self$add_nand(source_1_node = nand2, source_1_bit = "o1", source_2_node = nand3, source_2_bit = "o1");

      # Apply NAND to the two NANDs
      nand5 <- self$add_nand(
        source_1_node = nand1, source_1_bit = "o1",
        source_2_node = nand4, source_2_bit = "o1",
        target_node = self, target_bit = "o1");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
