library(R6);
#install.packages("data.table");
library(data.table);

#' CompositeTree
#'
#' A CompositeTree is an algorithm that is composed of AbstractNodes.
#' A CompositeTree should itself be a AbstractNode, enabling complex and deep trees.
#' @export
CompositeTree <- R6Class(
  "CompositeTree",
  inherit = AbstractNode,
  public = list(
    # Private Members
    # Constructor
    initialize = function(
      node_id = NULL,
      input_dimension = NULL,
      output_dimension = NULL) {
      # Call the supercall constructor
      super$initialize(
        node_id = node_id,
        input_dimension = input_dimension,
        output_dimension = output_dimension);
      # Store private members
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
    get_inverse = function() {
      stop("ooops");
    },
    get_prettystring = function(){
      stop("ooops");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
