library(R6);

require(R6);

#' algo_1011 (R6 class)
#'
#' @description The logical algorithm with truth table 1011 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_1011_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 00 \tab 1\cr
#' 10 \tab 0\cr
#' 01 \tab 1\cr
#' 11 \tab 1
#'}
#'
#' @examples a <- algo_1011$new();
#' a$plot();
#' a$exec("10");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_10:algo_composite:algo_base.
#' @export
algo_1011 <- R6Class(
  "algo_1011",
  inherit = algo_composite,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT1011"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        algo_id = algo_id); #,
      #label = label,
      #...);

      # Apply NAND to the two input bits.
      nand1 <- self$add_nand(self, "i1", self, "i2");

      # Apply NAND to the first input + the above NAND.
      nand2 <- self$add_nand(self, "i1", nand1, "o1");

      # Pipe the final output.
      self$set_inner_edge(nand2, "o1", self, "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)