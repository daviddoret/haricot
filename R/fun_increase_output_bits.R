#' increase_output_bits
#'
#' Increase the number of output bits in a composite algorithm.
#'
#' @examples # R function style:
#' a1 <- algo_composite$new(3,5);
#' increase_output_bits(a1, 2);
#'
#' # R6 method style:
#' a2 <- algo_composite$new(3,5);
#' a2$increase_output_bits(2);
#'
#' @param algo A composite algorithm (R6 Class algo_composite)
#' @param n (conditional, default = 1) The number of output bits to be added (integer)
#' @return The composite algorithm (for function chaining)
#' @export
increase_output_bits = function(algo, n = NULL, ...) {

  if(is.null(n)){
    n <- 1;
  };
  n <- as.integer(n);
  if(!is.integer(n)){
    flog.error("n is not an integer");
    stop();
  }
  if(!(n > 0)){
    flog.error("n is not greater than 0");
    stop();
  }

  previous_dim_o <- algo$get_dim_o();
  new_dim_o <- previous_dim_o + n;

  # Add the corresponding vertices in the DAG.
  bit_numbers <- (previous_dim_o + 1) : new_dim_o;
  bit_names <- baptize_algo_bit(OUTPUT_PREFIX, bit_numbers);
  vertices_names <- paste0(algo$get_algo_id(), NAMESPACE_SEPARATOR, bit_names);
  dag <- algo$get_dag();
  dag <- dag %>%
    add_vertices(
      nv = length(bit_numbers),
      bit = bit_names,
      color = get_opt("VERTIX_OUTPUTBIT_BGCOLOR", ...),
      label = bit_names,
      name = vertices_names,
      algo_id = algo$get_algo_id(),
      push_execution_value = list(), # A vector of pushed execution values.
      shape = get_opt("VERTIX_OUTPUTBIT_SHAPE", ...),
      size = get_opt("VERTIX_OUTPUTBIT_SIZE", ...),
      type = get_opt("VERTIX_OUTPUTBIT_TYPE", ...));
  algo$set_dag(dag);

  # Adapt the output dimension property.
  algo$system_set_dim_o(new_dim_o);

  # Chaining
  return(algo);

}
