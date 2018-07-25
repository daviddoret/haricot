library(R6);

#' SuperNode_ThinkingLoudly.
#'
#' SuperNode is an abstract class.
#' It defines the interfaces required for subclasses,
#' that must be compatible as a node in a SuperTree.
#' Its basic definition is that it comprises:
#' - a number of input bits,
#' - a number of output bits,
#' - a method to run its algorithm for a given input and return its output,
#' - utility methods of general usage.
#' @export
SuperNode_ThinkingLoudly <- R6Class(
  "SuperNode",
  public = list(
    # Private Members
    node_id = NULL,
    input_dimension = NULL,
    output_dimension = NULL,
    initialize = function(
      node_id = NULL,
      input_dimension = NULL,
      output_dimension = NULL) {
      # Store private members
      self$node_id <- node_id;
      self$input_dimension <- input_dimension;
      self$output_dimension <- output_dimension;
      # Initializes the input nodes
      for(input_node_number in 1 : input_dimension){
        node_id <- paste0("i", input_node_number);
        self$set_input_node(node_id);
      }
      # Initializes the output nodes
      for(output_node_number in 1 : output_dimension){
        node_id <- paste0("o", output_node_number);
        self$set_output_node(node_id);
      }
    },
    do_apply_algorithm = function(input) {
    },
    do_plot = function() {
    },
    do_randomize_outputs = function() {
    },
    get_input_dimension = function() {
      return(self$input_dimension);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {
    },
    get_output_dimension = function() {
      return(self$output_dimension);
    },
    get_prettystring = function(){
      return(paste(self$logical_datatable[,"prettystring"], collapse = "\n"));
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
