require(R6);
#install.packages("rlang");
require(rlang);
require(igraph);

#' AlgoNode (R6 class)
#'
#' AlgoNode is an abstract class that should be inherited from.
#' It defines the interfaces required for objects that must be compatible as a node in a CompositeTree.
#' Its basic definition is that it comprises:
#' - identification, labeling and other decorative information,
#' - a number of input bits,
#' - a number of output bits,
#' - a method to run its algorithm for a given input and return its output,
#' - utility methods of general usage.
#' @export
AlgoNode <- R6Class(
  "AlgoNode",
  private = list(
    input_dimension = NULL,
    output_dimension = NULL,
    node_id = NULL,
    label = NULL
  ),
  public = list(
    # Private Members
    initialize = function(
      input_dimension,
      output_dimension,
      node_id = NULL,
      label = NULL,
      ...) {
      # Store private members
      private$input_dimension <- input_dimension;
      private$output_dimension <- output_dimension;
      if(is.null(node_id)){ node_id <- get_node_guid(); }
      private$node_id <- node_id;
      if(is.null(label)){ label <- "algo"; }
      private$label <- label;
    },
    do_convert_to_igraph = function(...){
      return(do_convert_AlgoNode_to_igraph(node = self, ...));
    },
    do_execute = function(input) {
      stop("This method is abstract, please implement it in the subclass.");
    },
    do_plot = function() {
      plot_AlgoNode(self);
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
      stop("This method is abstract, please implement it in the subclass.");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
