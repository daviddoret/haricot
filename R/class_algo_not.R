require(R6);

#' algo_not (R6 class)
#'
#' @description The NOT logical algorithm implemented as a NAND-composite.
#' In practice, since NOT has the truthtable {1,0}, this class inherits from Algo10.
#'
#' @section Graph:
#' {\figure{algo_not_graph.png}{Graph of the algorithm}}
#'
#' @section Truth table:
#' \tabular{ll}{
#' \strong{input} \tab \strong{output}\cr
#' 0 \tab 1\cr
#' 1 \tab 0
#'}
#'
#' @examples a <- algo_not$new();
#' a$plot();
#' a$exec("1");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_not:algo_10:algo_composite:algo_base.
#' @export
algo_not <- R6Class(
  "algo_not",
  inherit = algo_10,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      if(is.null(label)){ label <- "NOT"; }
      super$initialize(
        algo_id = algo_id,
        label = label);
        #...);
    }
  )
)
