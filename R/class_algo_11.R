require(R6);
#' algo_11 (R6 class)
#'
#' @description The logical algorithm with truth table 01 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_11_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 0 \tab 1\cr
#' 1 \tab 1
#'}
#'
#' @examples a <- algo_11$new();
#' a$do_plot();
#' a$do_execute("1");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_11:algo_composite:algo_base.
#' @export
algo_11 <- R6Class(
  "algo_11",
  inherit = algo_composite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 1;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT11"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Design the algorithm.

      # Inverse input bit 1
      nand1 <- self$add_nand(self, "i1", self, "i1");

      # NAND input bit 1 with its inverse, automatically yielding TRUE
      nand2 <- self$add_nand(self, "i1", nand1, "o1");

      # Pipe the result.
      self$set_inner_edge(nand2, "o1", self, "o1");
    }
  )
)
