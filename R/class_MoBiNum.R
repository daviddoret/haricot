library(R6);

MoBiNum <- R6Class(
  "MoBiNum",
  public = list(
    # Private Members
    logical_vector = NULL,
    initialize = function(input, dimension = 1) {
      self$logical_vector <- convert_any_to_logical_vector(input);
    },
    convert_to_character = function(){
      return(convert_logical_vector_to_character(self$logical_vector));
    },
    convert_to_logical_vector = function(){
      return(self$logical_vector);
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
    },
    print = function(){
      print(self$get_prettystring());
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
