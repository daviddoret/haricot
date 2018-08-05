require(R6);
#' AlgoTT10 (R6 class)
#'
#' @description The logical algorithm with truth table 01 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_tt10_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 0 \tab 1\cr
#' 1 \tab 0
#'}
#'
#' @examples a <- AlgoTT10$new();
#' a$do_plot();
#' a$do_execute("1");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoTT10:AlgoComposite:AlgoNode.
#' @export
AlgoTT10 <- R6Class(
  "AlgoTT10",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 1;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT10"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      # Design the algorithm.
      nand1 <- self$add_nand(self, "i1", self, "i1");

      # Pipe the result.
      self$set_inner_edge(nand1, "o1", self, "o1");
    }
  )
)
