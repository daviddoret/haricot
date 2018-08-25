#' Execute the 0 constant algorithm on no or a null input.
#'
#' @description ...
#'
#' @examples # R function style:
#' exec_algo_0(algo, input);
#'
#' # R6 method style:
#' a <- algo_0$new();
#' a$exec(input);
#'
#' @param algo A 0 constant algorithm (R6 Class algo_0)
#' @param input (conditional) The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input, default type: logical vector)
#' @export
exec_algo_0 = function(algo, input = NULL, ...) {

  if(is.null(input)){
    input <- logical(0);
  }

  output_logical_vector <- c(FALSE);

  if(is(input, "bnum")){
    return(bnum$new(output_logical_vector));
  } else if(is(input, "logical")){
    return(output_logical_vector);
  } else if(is(input, "character")){
    return(convert_logical_vector_to_character(output_logical_vector));
  } else {
    # Oooops!
    stop(input);
  }
}
