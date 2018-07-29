library(R6);

#' AbstractNode.
#'
#' AbstractNode is an abstract class.
#' It defines the interfaces required for subclasses,
#' that must be compatible as a node in a CompositeTree.
#' Its basic definition is that it comprises:
#' - a number of input bits,
#' - a number of output bits,
#' - a method to run its algorithm for a given input and return its output,
#' - utility methods of general usage.
#' @export
AbstractNode <- R6Class(
  "AbstractNode",
  public = list(
    # Private Members
    node_id = NULL,
    input_dimension = NULL,
    output_dimension = NULL,
    initialize = function(
      node_id = NULL,
      input_dimension,
      output_dimension) {
      if(is.null(node_id)){
        node_id <- get_node_guid();
      }
      # Store private members
      self$node_id <- node_id;
      self$input_dimension <- input_dimension;
      self$output_dimension <- output_dimension;
    },
    do_apply_algorithm = function(input) {
      stop("This method is abstract, please implement it in the subclass.");
    },
    do_plot = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    do_randomize_outputs = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_input_dimension = function() {
      return(self$input_dimension);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_node_id = function(){
      return(self$node_id);
    },
    get_output_dimension = function() {
      return(self$output_dimension);
    },
    get_prettystring = function(){
      stop("This method is abstract, please implement it in the subclass.");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
