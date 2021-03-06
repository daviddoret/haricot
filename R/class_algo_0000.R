library(R6);

require(R6);

#' algo_0000 (R6 class)
#'
#' @description The logical algorithm with truth table 0000 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_0000_graph.png}{Graph of the algorithm}}
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
#' @examples a <- algo_0000$new();
#' a$plot();
#' a$exec("10");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_10:algo_composite:algo_base.
#' @export
algo_0000 <- R6Class(
  "algo_0000",
  inherit = algo_composite,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      dim_i <- 2;
      dim_o <- 1;
      if(is.null(label)){ label <- "TT0000"; }
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        ...);

      # Invert the first input bit.
      nand1 <- self$add_nand(
        source_1_node = self,
        source_1_bit = "i1",
        source_2_node = self,
        source_2_bit = "i1",
        ...);

      # NAND the first input bit with its inverse.
      # The output is unconditionnaly TRUE.
      nand2 <- self$add_nand(
        source_1_node = self,
        source_1_bit = "i1",
        source_2_node = nand1,
        source_2_bit = "o1",
        ...);

      # Invert output to get an unconditional FALSE.
      nand3 <- self$add_nand(
        source_1_node = nand2,
        source_1_bit = "o1",
        source_2_node = nand2,
        source_2_bit = "o1",
        ...);

      # Pipe the final output.
      self$set_dag_edge(
        source_node = nand3,
        source_bit = "o1",
        target_node = self,
        target_bit = "o1",
        ...);
    },
    do_randomize_outputs = function(...) {
      stop("Not supported");
    }
  )
)
