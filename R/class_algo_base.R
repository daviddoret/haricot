require(R6);
require(rlang);
require(igraph);

#' algo_base (R6 class)
#'
#' algo_base is the base abstract class that implements the concept of algorithm in \eqn{\mathbb{A}^{\mathbb{B}}}.
#' Instances of classes that implement algo_base may act as a component in an algorithm composite.
#' In addition to the formal components expected from the definition of an algorithm in \eqn{\mathbb{A}^{\mathbb{B}}}, the class provides complementary services such as unique IDs, labels, etc.
#'
#' @seealso \code{\link{algorithms_in_ab}}
#' @seealso \code{\link{notation}}
#'
#' @export
algo_base <- R6Class(
  "algo_base",
  private = list(
    input_dimension = NULL,
    output_dimension = NULL,
    algo_id = NULL,
    label = NULL
  ),
  public = list(
    # Private Members
    initialize = function(
      input_dimension,
      output_dimension,
      algo_id = NULL,
      label = NULL,
      ...) {
      # Store private members
      private$input_dimension <- input_dimension;
      private$output_dimension <- output_dimension;
      if(is.null(algo_id)){ algo_id <- get_node_guid(); }
      private$algo_id <- algo_id;
      if(is.null(label)){ label <- "algo"; }
      private$label <- label;
    },
    convert_to_igraph = function(...){
      return(convert_algo_base_to_igraph(node = self, ...));
    },
    convert_to_algo_ = function(...){
      return(convert_algo_base_to_algo_(algo = self, ...));
    },
    exec = function(input) {
      stop("This method is abstract, please implement it in the subclass.");
    },
    plot = function() {
      plot_algo_base(self);
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
    get_algo_id = function(){
      return(private$algo_id);
    },
    get_output_dimension = function() {
      return(private$output_dimension);
    },
    get_prettystring = function(){
      stop("This method is abstract, please implement it in the subclass.");
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    },
    is_constant = function(){
      if(self$get_input_dimension() == 0){
        # By definition, an algorithm
        # that does not have any input
        # is a constant.
        return(TRUE);
      } else {
        return(FALSE);
      }
    }
  )
)
