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
    dim_i = NULL,
    dim_o = NULL,
    algo_id = NULL,
    label = NULL,
    is_atomic = NULL
  ),
  public = list(
    # Private Members
    initialize = function(
      dim_i,
      dim_o,
      algo_id = NULL,
      label = NULL,
      is_atomic = NULL,
      ...) {
      # Store private members
      private$dim_i <- dim_i;
      private$dim_o <- dim_o;
      if(is.null(algo_id)){ algo_id <- get_node_guid(); }
      private$algo_id <- algo_id;
      if(is.null(label)){ label <- "algo"; }
      private$label <- label;
      if(is.null(is_atomic)){ is_atomic <- FALSE; }
      private$is_atomic <- is_atomic;
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
    plot = function(interactive = FALSE, ...) {
      plot_algo_base(self, interactive, ...);
    },
    do_randomize_outputs = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    from_json = function(json, ...){
      from_json(json, self, ...);
      # Chaining.
      return(self);
    },
    get_dim_i = function() {
      return(private$dim_i);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_dim_i());
    },
    get_is_atomic = function() {
      return(private$is_atomic);
    },
    get_inverse = function() {
      stop("This method is abstract, please implement it in the subclass.");
    },
    get_label = function(){
      return(paste0(private$label));
    },
    get_algo_id = function(){
      return(private$algo_id);
    },
    get_dim_o = function() {
      return(private$dim_o);
    },
    get_prettystring = function(){
      stop("This method is abstract, please implement it in the subclass.");
    },
    is_constant = function(){
      if(self$get_dim_i() == 0){
        # By definition, an algorithm
        # that does not have any input
        # is a constant.
        return(TRUE);
      } else {
        return(FALSE);
      }
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    },
    set_algo_id = function(id, ...){
      private$algo_id <- id;
    },
    set_label = function(label, ...){
      private$label <- label;
    },
    to_json = function(...){
      return(to_json(self, ...));
    }
  )
)
