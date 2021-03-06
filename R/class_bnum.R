require(R6);
require(futile.logger);

#' bnum (R6 class)
#'
#' The R6 class implementation of the \emph{\href{../articles/modular_binary_number.html}{modular binary number}} concept.
#'
#' @examples # Construction from character string
#' n1 <- bnum$new("0101111000");
#' n1$plot();
#'
#' # Construction from logical vector
#' n2 <- bnum$new(c(TRUE,FALSE,FALSE,TRUE));
#' n2$plot();
#'
#' # Construction with dimension, defaulting to 0
#' n3 <- bnum$new(dim = 5);
#' n3$plot();
#'
#' @param input (conditional) An initialization value (character | logical vector)
#' @param dim (conditional) The desired dimension of the binary number (integer)
#' @param ... For future usage.
#' @return An object instance of class bnum.
#' @export
bnum <- R6Class(
  "bnum",
  public = list(
    # Private Members
    logical_vector = NULL,
    initialize = function(
      input = NULL,
      dim = NULL,
      ...) {
      if(is.null(input) & !is.null(dim)){
        self$logical_vector <- rep(FALSE, dim);
      } else if(!is.logical(input)){
        self$logical_vector <- input;
      } else {
        self$logical_vector <- convert_any_to_logical_vector(input, dim, ...);
      }
    },
    convert_to_character = function(){
      return(convert_logical_vector_to_character(self$logical_vector));
    },
    convert_to_logical_vector = function(){
      return(self$logical_vector);
    },
    copy = function(){
      return(bnum$new(self$logical_vector));
    },
    get_bit = function(bit_position){
      return(self$logical_vector[bit_position]);
    },
    get_dimension = function() {
      return(length(self$logical_vector));
    },
    get_equal_0 = function() {
      return(!any(self$logical_vector));
    },
    get_first_bit = function(){
      return(self$get_bit(1));
    },
    get_last_bit = function(){
      return(self$get_bit(self$get_dimension()));
    },
    get_logical_vector = function() {
      return(self$logical_vector);
    },
    get_prettystring = function() {
      return(paste(as.numeric(self$logical_vector), collapse=""));
    },
    do_increment = function(){
      # do_increment is a function that adds 1 to a "binary number".
      # because we use modular arithmetic, if all bit values are equal to 1 and we increment_binum,
      # we should end with all 0s.
      # here, we define the left most bit as the least significant bit.
      index <- 1;
      incremented <- FALSE;
      while(index <= self$get_dimension() & !incremented) {
        if(self$get_bit(index)){
          self$set_bit_0(index);
        } else {
          self$set_bit_1(index);
          incremented <- TRUE;
        }
        index <- index + 1;
      }
      # If the incrementation failed here,
      # we end up with all 0s.
      # This is intentional as we are using modular arithmetic.

      # Chaining.
      return(self);
    },
    format = function(){
      return(self$get_prettystring());
    },
    plot = function(...){
      plot_bnum(self, ...);
    },
    print = function(){
      print(self$get_prettystring());
    },
    randomize = function() {
      # Randomizes the value of the bnum.
      random_logical_vector <- sample(x = c(FALSE, TRUE), size = self$get_dimension(), replace = TRUE);
      # Replace the inner logical vector with the random one.
      self$logical_vector <- random_logical_vector;
      # Enables chaining
      return(self);
    },
    set_bit = function(bit_position, input){
      logical <- convert_any_to_logical_vector(input);
      self$logical_vector[bit_position] <- logical;
    },
    set_bit_0 = function(bit_position){
      self$set_bit(bit_position = bit_position, input = FALSE);
    },
    set_bit_1 = function(bit_position){
      self$set_bit(bit_position = bit_position, input = TRUE);
    }
  )
)

