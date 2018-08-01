require(R6);
#install.packages("rlang");
require(rlang);
require(igraph);

#' AlgoNAND
#'
#' The atomic NAND algorithm node.
#' @export
AlgoNAND <- R6Class(
  "AlgoNAND",
  inherit = AlgoNode,
  private = list(
  ),
  public = list(
    # Private Members
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      input_dimension <- 2;
      output_dimension <- 1;
      if(is.null(label)){ label <- "NAND"; }
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id,
        label = label,
        ...);
    },
    do_execute = function(input, ...) {
      return(do_execute_AlgoNAND(algo = self, input, ...));
    },
    do_plot = function() {
      do_plot_AlgoNode(self);
    },
    do_randomize_outputs = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_input_dimension = function() {
      return(private$input_dimension);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_label = function(){
      return(private$label);
    },
    get_node_id = function(){
      return(private$node_id);
    },
    get_output_dimension = function() {
      return(private$output_dimension);
    },
    get_prettystring = function(){
      return("nand");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
