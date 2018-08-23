#' Execute a truth table algorithm on a given input.
#'
#' @description ...
#'
#' @examples # R function style:
#' exec_algo_tt(algo, input);
#'
#' # R6 method style:
#' algo$exec(input);
#'
#' @param algo A truth table algorithm (R6 Class algo_tt)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
exec_algo_tt = function(algo, input, ...) {

  log(fun = "exec_algo_tt", algo = algo, input = input, ...);

  input_logical_vector <- convert_any_to_logical_vector(input);

  if(length(input_logical_vector) != algo$get_dim_i()){
    stop("algo input dimension <> input dimension");
  }

  # Find the index position in the matrix
  input_position <- convert_logical_vector_to_position(input_logical_vector);
  # Return the corresponding row
  output_logical_vector <- algo$get_logical_matrix()[input_position,];

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
