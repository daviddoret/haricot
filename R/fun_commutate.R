#' commutate
#'
#' ...
#'
#' @examples # Switch two simple algos:
#' require(haricot);
#' algo_0 <- algo_01$new();
#' algo_0$plot();
#' algo_1 <- algo_10$new();
#' algo_1$plot();
#' commutated <- commutate(algo_0, algo_1);
#' commutated$exec("00");
#' commutated$exec("10");
#' commutated$exec("01");
#' commutated$exec("11");
#' commutated$plot();
#'
#' @param algo_0 A component algorithm to be commutated to when e == 0 (algo_base)
#' @param algo_1 A component algorithm to be commutated to when e == 1 (algo_base)
#' @param ... For future usage
#' @return A component algorithm whose inner logic commutates algo_0 and algo_1 based on e
#' @export
commutate <- function(
  algo_0,
  algo_1,
  ...){

  if(!is(algo_0, "algo_base")) { stop("algo_0 does not implement algo_base"); };
  if(!is(algo_1, "algo_base")) { stop("algo_1 does not implement algo_base"); };

  if(algo_0$get_input_dimension() != algo_1$get_input_dimension()) { stop("Input dimensions are not identical"); };
  if(algo_0$get_output_dimension() != algo_1$get_output_dimension()) { stop("Output dimensions are not identical"); };

  input_dimension <- algo_0$get_input_dimension() + 1;
  output_dimension <- algo_0$get_output_dimension();
  extra_bit <- paste0(INPUT_PREFIX, input_dimension);

  commutated <- algo_composite$new(input_dimension, output_dimension);

  commutated$set_inner_node(algo_0);
  commutated$set_inner_node(algo_1);

  # Loop on the component algo bits
  if((input_dimension - 1)){
    # But only if the sub algorithms are not constants with input dim 0.
    for(position in 1 : (input_dimension - 1)){
      # Pipes switch input bits to component bits.
      # Like this, both component algorithms will be unconditionally executed.
      commutated$set_inner_edge(commutated, paste0(INPUT_PREFIX, position), algo_0, paste0(INPUT_PREFIX, position));
      commutated$set_inner_edge(commutated, paste0(INPUT_PREFIX, position), algo_1, paste0(INPUT_PREFIX, position));
    }
  }

  # Loop on the component algo bits
  for(position in 1 : output_dimension){

    # If extra_bit == 0, keep algo_0 output bit. Otherwise, set it to 0.
    circuit_0 <- algo_0100$new(label = "circuit_0");
    commutated$set_inner_node(circuit_0);
    commutated$set_inner_edge(algo_0, paste0(OUTPUT_PREFIX, position), circuit_0, "i1");
    commutated$set_inner_edge(commutated, extra_bit, circuit_0, "i2");

    # If extra_bit == 1, keep algo_1 output bit. Otherwise, set it to 0.
    circuit_1 <- algo_0001$new(label = "circuit_1");
    commutated$set_inner_node(circuit_1);
    commutated$set_inner_edge(algo_1, paste0(OUTPUT_PREFIX, position), circuit_1, "i1");
    commutated$set_inner_edge(commutated, extra_bit, circuit_1, "i2");

    # OR the two switches
    commutator <- algo_or$new(label = "commutator");
    commutated$set_inner_node(commutator);
    commutated$set_inner_edge(circuit_0, "o1", commutator, "i1");
    commutated$set_inner_edge(circuit_1, "o1", commutator, "i2");

    # Pipe the OR to the final output bit
    commutated$set_inner_edge(commutator, "o1", commutated, paste0(OUTPUT_PREFIX, position));

  }

  return(commutated);

}
