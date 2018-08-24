require(futile.logger);
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
exec_algo_tt = function(algo, input = NULL, ...) {

  # Data validation.
  if(!is(algo, "algo_tt")){
    flog.error("exec_algo_tt: algo is not of algo_tt class");
    };
  if(is.null(input)){
    # Input is not mandatory, because the algorithm may be a constant with input dimension 0.
    # But we need to know the requested return type.
    # Strong assumption: in this situation, we default to the bnum type.
    flog.warn("exec_algo_tt: missing|NULL|NA input received --> default set to bnum$new(dim=0)");
    input <- logical(0);
    };
  input_logical_vector <- convert_any_to_logical_vector(input);
  if(length(input_logical_vector) != algo$get_dim_i()){
    flog.error("algo input dimension <> input dimension");
    };

  input_position <- NULL;
  if(algo$get_dim_i() == 0){
    # This is a constant, the logical matrix contains only 1 row.
    input_position <- 1;
  } else {
    # Find the index position in the matrix
    input_position <- convert_logical_vector_to_position(input_logical_vector);
  }
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
