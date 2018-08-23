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
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_dim_i = function() {
      return(private$dim_i);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_dim_i());
    },
    get_inverse = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_label = function(){
      return(private$label);
    },
    get_algo_id = function(){
      return(private$algo_id);
    },
    get_dim_o = function() {
      return(private$dim_o);
    },
    get_prettystring = function(){
      return("nand");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
