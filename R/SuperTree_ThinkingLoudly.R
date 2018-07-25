library(R6);
#install.packages("data.table");
library(data.table);

#' SuperTree_ThinkingLoudly.
#'
#' A SuperTree is an algorithm that is composed of SuperNodes.
#' A SuperTree should itself be a SuperNode, enabling complex and deep trees.
#' @export
SuperTree_ThinkingLoudly <- R6Class(
  "SuperTree_ThinkingLoudly",
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
