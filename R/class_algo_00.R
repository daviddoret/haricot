require(R6);
#' algo_00 (R6 class)
#'
#' @description The logical algorithm with truth table 01 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_00_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 0 \tab 0\cr
#' 1 \tab 0
#'}
#'
#' @examples a <- algo_00$new();
#' a$plot();
#' a$exec("1");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_00:algo_composite:algo_base.
#' @export
algo_00 <- R6Class(
  "algo_00",
  inherit = algo_composite,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      dim_i <- 1;
      dim_o <- 1;
      if(is.null(label)){ label <- "TT00"; }
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        ...);

      # Design the algorithm.

      # Inverse input bit 1
      nand1 <- self$add_nand(self, "i1", self, "i1", ...);

      # NAND input bit 1 with its inverse, automatically yielding TRUE
      nand2 <- self$add_nand(self, "i1", nand1, "o1", ...);

      # inverse the NAND
      nand3 <- self$add_nand(nand2, "o1", nand2, "o1", ...);

      # Pipe the result.
      self$set_dag_edge(nand3, "o1", self, "o1", ...);
    }
  )
)
