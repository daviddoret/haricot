require(R6);

#' algo_01 (R6 class)
#'
#' @description The logical algorithm with truth table 01 implemented as a NAND-composite.
#'
#' @section Graph:
#' {\figure{algo_01_graph.png}{Graph of the algorithm}}
#'
#' @examples a <- algo_01$new();
#' a$plot();
#' a$exec("1");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_10:algo_composite:algo_base.
#' @export
algo_01 <- R6Class(
  "algo_01",
  inherit = algo_composite,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 1;
      output_dimension <- 1;
      if(is.null(label)){ label <- "TT01"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        algo_id = algo_id,
        label = label);
      #...);

      self$set_inner_edge(source_node = self, source_bit = "i1", target_node = self, target_bit = "o1");
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)