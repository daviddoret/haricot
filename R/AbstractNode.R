require(R6);
#install.packages("rlang");
require(rlang);
require(igraph);

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
    input_dimension = NULL,
    output_dimension = NULL
  ),
  public = list(
    # Private Members
    initialize = function(
      input_dimension,
      output_dimension) {
      # Store private members
      private$input_dimension <- input_dimension;
      private$output_dimension <- output_dimension;
    },
    do_apply_algorithm = function(input) {
      stop("This method is abstract, please implement it in the subclass.");
    },
    do_plot = function() {
      plot(self$get_graph());
    },
    do_randomize_outputs = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_graph = function(){
      # Default igraph representation of an AbstractNode.
      g <- make_empty_graph(directed = TRUE) %>%
        add_vertices(
          nv = self$get_input_dimension(),
          bit_id = paste0("i", 1:self$get_input_dimension()),
          label = paste0("i", 1:self$get_input_dimension()),
          name = paste0("_self",".",paste0("i", 1:self$get_input_dimension())),
          node_id = "_self",
          type = "i") %>%
        add_vertices(
          nv = 1,
          bit_id = NA,
          label = NA,
          name = "_self",
          node_id = "_self",
          type = "x") %>%
        add_vertices(
          nv = self$get_output_dimension(),
          bit_id = paste0("o", 1:self$get_output_dimension()),
          label = paste0("o", 1:self$get_output_dimension()),
          name = paste0("_self",".",paste0("o", 1:self$get_output_dimension())),
          node_id = "_self",
          type = "o") %>%
        add_edges(
          c(
            rbind(
              1:self$get_input_dimension(),
              rep(self$get_input_dimension() + 1, self$get_input_dimension())))) %>%
        add_edges(
          c(
            rbind(
              rep(self$get_input_dimension() + 1, self$get_output_dimension()),
              self$get_input_dimension() + 1 + 1:self$get_output_dimension())));
      return(g);
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
