require(R6);
require(rlang);
require(igraph);

#' algo_nand (R6 class)
#'
#' @description Any algorithm may be built from the atomic NAND algorithm.
#'
#' @section Graph:
#' {\figure{algo_nand_graph.png}{Graph of the algorithm}}
#'
#' @examples a1 <- algo_nand$new();
#' a1$plot();
#' a1$exec("01");
#'
#' @param algo_id A technical unique identifier for the algorithmic node. If missing, a GUID will be created. (character)
#' @param label A meaningful label for the algorithmic node. Keep it short to let it display properly on graph plots. Default: "NAND". (character)
#' @param ... For future usage.
#' @return An object instance of class algo_nand:algo_base.
#' @export
algo_nand <- R6Class(
  "algo_nand",
  inherit = algo_base,
  private = list(
  ),
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      dim_i <- 2;
      dim_o <- 1;
      if(is.null(label)){ label <- "NAND"; }
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        is_atomic = TRUE,
        ...);
    },
    exec = function(input, ...) {
      log(object = self, method = "exec", input = input, ...);
      return(exec_algo_nand(algo = self, input, ...));
    },
    plot = function() {
      plot_algo_base(self);
    },
    do_randomize_outputs = function() {
      stop("Not supported, the logic of this class is read-only");
    },
    get_inverse = function() {
      stop("Not supported, the logic of this class is read-only");
    },
    get_prettystring = function(){
      return("nand");
    }
  )
)
