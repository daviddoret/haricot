#' Execute the NAND algorithm on a given input.
#'
#' @description ...
#'
#' @examples # R function style:
#' execute_algo_nand(algo, input);
#'
#' # R6 method style:
#' algo$exec(input);
#'
#' @param algo A NAND algorithm (R6 Class algo_nand)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
exec_algo_nand = function(algo, input) {
  # Applies the TruthTable algorithm and returns its output.
  # Returns a type that is consistent with the type of the input.
  input_logical_vector <- convert_any_to_logical_vector(input);

  output_logical_vector <- !(input_logical_vector[1] & input_logical_vector[2])

  if(is(input, "logical")){
    return(output_logical_vector);
  } else if(is(input, "character")){
    return(convert_logical_vector_to_character(output_logical_vector));
  } else if(is(input, "bnum")){
    return(bnum$new(output_logical_vector));
  } else {
    # Oooops!
    stop(input);
  }
}
