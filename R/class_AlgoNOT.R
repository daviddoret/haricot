require(R6);

#' AlgoNOT (R6 class)
#'
#' @description The NOT logical algorithm implemented as a NAND-composite.
#' In practice, since NOT has the truthtable {1,0}, this class inherits from Algo10.
#'
#' @section Graph:
#' {\figure{algo_not_graph.png}{Graph of the algorithm}}
#'
#' @usage a <- AlgoNOT$new();
#' a$do_plot();
#' a$do_execute("1");
#'
#' @param node_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class AlgoNOT:Algo10:AlgoComposite:AlgoNode.
#' @export
AlgoNOT <- R6Class(
  "AlgoNOT",
  inherit = Algo10,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      if(is.null(label)){ label <- "NOT"; }
      super$initialize(
        node_id = node_id); #,
        #label = label,
        #...);
    }
  )
)
