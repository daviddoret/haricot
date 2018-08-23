#' convert_tt_constant_to_composite
#'
#' @description Convert a constant truth table algorithm,
#' i.e. a truth table algorithm of input dimension 1,
#' and of arbitrary output dimension > 0,
#' and returns a composite algorithm
#' that reproduce the equivalent logic
#' but with only atomic NANDs and or atomic constants.
#'
#' @examples ...
#'
#' @param algo A truth table algorithm (R6 Class algo_tt)
#' @return The equivalent composite algorithm (R6 Class composite)
#' @export
convert_tt_constant_to_composite <- function(algo){

  if(!is(algo, "algo_tt")){ stop("algo is not of algo_tt class"); };
  if(!algo$get_dim_i() == 0) { stop("algo is not of input dimension 0"); };

  dim_i <- 0;
  dim_o <- algo$get_dim_o();

  composite <- algo_composite$new(dim_i, dim_o);

  logical_matrix <- algo$get_logical_matrix();

  for(output_bit_position in 1 : length(logical_matrix)){
    output_bit_value <- logical_matrix[output_bit_position];
    if(output_bit_value){
      # Create a constant sub algorithm of correct value.
      sub_algo <- algo_1$new();
      composite$set_inner_node(sub_algo);
      # Pipe the constant to the corresponding output bit.
      composite$set_inner_edge(
        sub_algo,
        baptize_algo_bit(OUTPUT_PREFIX, 1),
        composite,
        baptize_algo_bit(OUTPUT_PREFIX, output_bit_position));
    } else {
      # Create a constant sub algorithm of correct value.
      sub_algo <- algo_0$new();
      composite$set_inner_node(sub_algo);
      # Pipe the constant to the corresponding output bit.
      composite$set_inner_edge(
        sub_algo,
        baptize_algo_bit(OUTPUT_PREFIX, 1),
        composite,
        baptize_algo_bit(OUTPUT_PREFIX, output_bit_position));
    };
  };

  return(composite);
}
