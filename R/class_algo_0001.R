library(R6);

require(R6);

#' algo_and, algo_0001 (R6 class)
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
      dim_i <- 2;
      dim_o <- 1;
      if(is.null(label)){ label <- "TT0001"; }
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        ...);

      # Apply NAND to the two input bits.
      nand1 <- self$add_nand(self, "i1", self, "i2", ...);

      # Inverse the result.
      nand2 <- self$add_nand(nand1, "o1", nand1, "o1", ...);

      # Pipe the final output.
      self$set_dag_edge(nand2, "o1", self, "o1", ...);
    },
    do_randomize_outputs = function(...) {
      stop("Not supported");
    }
  )
)
