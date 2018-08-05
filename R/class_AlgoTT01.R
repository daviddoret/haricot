require(R6);

#' AlgoTT01 (R6 class)
#'
#' @description The logical algorithm with truth table 01 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_tt01_graph.png}{Graph of the algorithm}}
#'
#' @examples a <- AlgoTT01$new();
#' a$do_plot();
#' a$do_execute("1");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoTT10:AlgoComposite:AlgoNode.
#' @export
AlgoTT01 <- R6Class(
  "AlgoTT01",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 1;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT01"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id); #,
      #label = label,
      #...);

      self$set_inner_edge(source_node = self, source_bit = "i1", target_node = self, target_bit = "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
