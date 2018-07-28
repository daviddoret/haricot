library(R6);

#' TruthTable_FlexOutput
#'
#' To store and manage truthtables with an input of arbitrary size N and an output of arbitrary size M, we will use a logical matrix with size 2 ^ N rows and M columns.
#' It is not necessary to store the input of the truthtable in the truthtable data structure because we will use the row index position of the output to infer the input. For instance:
#' Row index position = 1 --> Input = binum of value 0.
#' Row index position = 2 --> Input = binum of value 1.
#' Row index position = 3 --> Input = binum of value 2.
#' ...
#' Row index position = 2 ^ N --> Input = binum of value 2 ^ N - 1.
#'
#' @examples
#' t1 <- TruthTable_FlexOutput$new(
#'   input_dimension = 2,
#'   output_dimension = 2);
#' t1$set_output(input = "01", output = "11");
#' t1$do_apply_algorithm("01");
#'
#' @export
TruthTable_FlexOutput <- R6Class(
  "TruthTable_FlexOutput",
  public = list(
    # Private Members
    input_dimension = NULL,
    output_dimension = NULL,
    logical_matrix = NULL,
    initialize = function(
      input_dimension,
      output_dimension) {
      init_value <- FALSE;
      # Store private members
      self$input_dimension <- input_dimension;
      self$output_dimension <- output_dimension;
      # First prepare a vector.
      v <- rep(init_value, 2 ^ input_dimension * output_dimension);
      # Transform the vector in a matrix.
      self$logical_matrix <- matrix(data = v, nrow = 2 ^ input_dimension, ncol = output_dimension, byrow = TRUE);
    },
    do_apply_algorithm = function(input) {
      # Applies the TruthTable algorithm and returns its output.
      # Returns a type that is consistent with the type of the input.
      input_logical_vector <- convert_any_to_logical_vector(input);
      input_position <- convert_logical_vector_to_position(input_logical_vector);
      output_logical_vector <- self$logical_matrix[input_position,];
      if(is(input, "logical")){
        return(output_logical_vector);
      } else if(is(input, "character")){
        return(convert_logical_vector_to_character(output_logical_vector));
      } else if(is(input, "BinaryNUmber_Modular")){
        return(BinaryNUmber_Modular$new(output_logical_vector));
      } else {
        # Oooops!
        stop(input);
      }
    },
    do_randomize_outputs = function() {
      # Randomizes the outputs of the TruthTable.
      # Build a random vector with enough items to fill in the truthtable
      random_logical_vector <- sample(x = c(FALSE, TRUE), size = 2 ^ self$get_input_dimension() * self$get_output_dimension(), replace = TRUE);
      # Convert the vector into a matrix
      random_logical_matrix <- matrix(data = random_logical_vector, nrow = 2 ^ self$get_input_dimension(), ncol = self$get_output_dimension(), byrow = TRUE);
      # Replace the logical matrix with the random one.
      self$logical_matrix <- random_logical_matrix;
    },
    get_input_dimension = function() {
      return(self$input_dimension);
      # Alternative solution: infer input_dimension from the logical_matrix.
      # return(log2(nrow(self$logical_matrix)));
    },
    get_input_size = function() {
      # Returns the number of different input values.
      # = 2 ^ input_dimension.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {

      # Find the dimensional input size of the original function.
      # This will become the output size of the inverse function.
      inverse_output_dimension <- self$get_input_dimension();

      # Find the dimensional output size of the original function.
      # This will become the input size of the inverse function.
      inverse_input_dimension <- self$get_output_dimension();

      # Name rows in the truthtable.
      # Like this we may reorder them and still match them to their original inputs.
      # Here we use the original position as the row identifier.
      logical_matrix_copy <- self$get_logical_matrix();
      rownames(logical_matrix_copy) <- 1 : nrow(logical_matrix_copy);

      # Get rid of duplicate outputs.
      logical_matrix_unique <- unique(logical_matrix_copy);

      # Initializes an empty truthtable.
      # We use all zeros as the default.
      inverse_truthtable <- TruthTable_FlexOutput$new(
        input_dimension = inverse_input_dimension,
        output_dimension = inverse_output_dimension);

      for(unique_position in 1:nrow(logical_matrix_unique)){
        input_position <- as.integer(rownames(logical_matrix_unique)[unique_position]);
        output_logical_vector <- logical_matrix_unique[unique_position,];
        # Apply this to the new truthtable
        inverse_output_logical_vector <- convert_position_to_logical_vector(input_position, size = inverse_output_dimension);
        inverse_input_logical_vector <- output_logical_vector;
        inverse_truthtable$set_output(inverse_input_logical_vector, inverse_output_logical_vector);
      }

      # Return the inverted truthtable.
      return(inverse_truthtable);

    },
    get_logical_matrix = function(){
      return(self$logical_matrix);
    },
    get_output_dimension = function() {
      return(self$output_dimension);
    },
    get_prettystring = function(){

      output_integer_vector <- as.numeric(self$get_logical_matrix());
      output_integer_matrix <- matrix(output_integer_vector, nrow = self$get_input_size());
      output_character_vector <- apply(output_integer_matrix, 1, paste, collapse = "");
      output_character_vector

      domain <- BinaryDomain$new(self$get_input_dimension());

      input_integer_vector <- as.numeric(domain$get_logical_matrix());
      input_integer_matrix <- matrix(input_integer_vector, nrow = self$get_input_size());
      input_character_vector <- apply(input_integer_matrix, 1, paste, collapse = "");
      input_character_vector

      final_character_vector <- c(input_character_vector, rep(" > ", self$get_input_size()), output_character_vector)
      final_character_matrix <- matrix(final_character_vector, ncol = 3, nrow = self$get_input_size())
      final_character <- paste(
        apply(final_character_matrix, 1, paste, collapse = ""),
        collapse = "\n");

      return(final_character);
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    },
    set_logical_matrix = function(logical_matrix){
      self$logical_matrix <- logical_matrix;
    },
    set_output = function(input, output){
      input_logical_vector <- convert_any_to_logical_vector(input);
      output_logical_vector <- convert_any_to_logical_vector(output);
      input_position <- convert_logical_vector_to_position(input_logical_vector);
      self$logical_matrix[input_position,] <- output_logical_vector;
    }
  )
)

