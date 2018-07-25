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
    },
    do_apply_algorithm = function(input) {
      stop("ooops");
    },
    do_plot = function() {
      stop("ooops");
    },
    do_randomize_outputs = function() {
      stop("ooops");
    },
    get_input_dimension = function() {
      return(self$input_dimension);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {
      stop("ooops");
    },
    get_output_dimension = function() {
      return(self$output_dimension);
    },
    get_prettystring = function(){
      stop("ooops");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
