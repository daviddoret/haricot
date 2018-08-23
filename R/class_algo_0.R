require(R6);

#' algo_0 (R6 class)
#'
#' @description The 0 constant algorithm. \cr
#' Viewed from another perspectie: the algorithm whose truth table is: {0}.
#'
#' @section Graph:
#' {\figure{algo_0_graph.png}{Graph of the algorithm}}
#'
#' @examples a <- algo_0$new();
#' a$plot();
#' a$exec();
#' a$exec(input = "");
#' a$exec(input = c(FALSE));
#' a$exec(input = bnum$new(dim=0));
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_0:algo_composite:algo_base.
#' @export
algo_0 <- R6Class(
  "algo_0",
  inherit = algo_base,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      dim_i <- 0;
      dim_o <- 1;
      if(is.null(label)){ label <- "0"; }
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        is_atomic = TRUE,
        ...);
    },
    do_randomize_outputs = function(...) {
      stop("Not supported");
    },
    exec = function(input = NA, ...){
      exec_algo_0(self, input, ...);
    },
    get_prettystring = function(...){
      return("0");
    }
  )
)
