require(R6);

#' algo_1 (R6 class)
#'
#' @description The 1 constant algorithm. \cr
#' Viewed from another perspective: the algorithm whose truth table is: {1}.
#'
#' @section Graph:
#' {\figure{algo_1_graph.png}{Graph of the algorithm}}
#'
#' @examples a <- algo_1$new();
#' a$plot();
#' a$exec();
#' a$exec(input = "");
#' a$exec(input = c(FALSE));
#' a$exec(input = bnum$new(dim=0));
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_1:algo_composite:algo_base.
#' @export
algo_1 <- R6Class(
  "algo_1",
  inherit = algo_base,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      dim_i <- 0;
      dim_o <- 1;
      if(is.null(label)){ label <- "1"; }
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        is_atomic = TRUE,
        ...);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    },
    exec = function(input = NULL, ...){
      exec_algo_1(self, input, ...);
    },
    get_prettystring = function(){
      return("1");
    }
  )
)
