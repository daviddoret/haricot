require(R6);
#install.packages("rlang");
require(rlang);

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
  private = list(
    node_id = NULL,
    node_label = NULL,
    node_notes = NULL,
    node_style = NULL,
    input_dimension = NULL,
    output_dimension = NULL
  ),
  public = list(
    # Private Members
    initialize = function(
      node_id = NULL,
      node_label = NULL,
      node_notes = NULL,
      node_style = NULL,
      input_dimension,
      output_dimension) {
      if(is_missing(node_id)){
        node_id <- get_node_guid();
      }
      if(is_missing(node_label)){
        node_label <- node_id;
      }
      if(is_missing(node_notes)){
        node_notes <- NULL;
      }
      if(is_missing(node_style)){
        node_style <- NULL;
      }
      # Store private members
      private$node_id <- node_id;
      private$node_label <- node_label;
      private$node_notes <- node_notes;
      private$node_style <- node_style;
      private$input_dimension <- input_dimension;
      private$output_dimension <- output_dimension;
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
      return(private$input_dimension);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {
      stop("This method is abstract, please implement it in the subclass.");
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
