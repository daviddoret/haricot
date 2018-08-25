require(R6);
require(futile.logger);

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
      } else if(!is.null(input) & is.null(dim)){
        self$logical_vector <- convert_any_to_logical_vector(input);
      } else {
        flog.error("bnum constructor not yet supported, sorry");
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
    get_first_bit = function(){
      return(self$get_bit(1));
    },
    get_last_bit = function(){
      return(self$get_bit(self$get_dimension()));
    },
    get_dimension = function() {
      return(length(self$logical_vector));
    },
    get_equal_0 = function() {
      return(!any(self$logical_vector));
    },
    get_logical_vector = function() {
      return(self$logical_vector);
    },
    get_prettystring = function() {
      return(paste(as.numeric(self$logical_vector), collapse=""));
      #return(self$convert_to_character());
    },
    do_increment = function(){
      # do_increment is a function that adds 1 to a "binary number".
      # because we use modulo arithmetic, if all bit values are equal to 1 and we increment_binum,
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
      # This is intentional as we are using modulo arithmetic.

      # Chaining.
      return(self);
    },
    format = function(){
      return(self$get_prettystring());
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

