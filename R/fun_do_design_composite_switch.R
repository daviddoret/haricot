#' do_design_composite_switch
#'
#' @description Definition: A composite switch, in this context, is a composite algorithm c1 composed of 2 component algorithms: c2 and c3.
#' c1, c2 and c3 have identical output dimensions.
#' c2 and c3 have identical input dimensions.
#' c1 has input dimension of c2's or c3's input dimension + 1, the "extra bit" e.
#' The composite switch algorithm functions in the following manner:
#' 1). it receives the normal c2 or c3 input + the extra bit e.
#' 2). if e == 0, it runs c2 and returns its output.
#' 3). if e == 1, it runs c3 and returns its output.
#' In summary, a composite switch is a practical construction that combines two algorithms and switch their execution with an extra bit.
#'
#' @examples # Switch two simple algos:
#' require(haricot);
#' algo_0 <- algo_01$new();
#' algo_0$do_plot();
#' algo_1 <- algo_10$new();
#' algo_1$do_plot();
#' algo_switch <- do_design_composite_switch(algo_0, algo_1);
#' algo_switch$do_execute("00");
#' algo_switch$do_execute("10");
#' algo_switch$do_execute("01");
#' algo_switch$do_execute("11");
#' algo_switch$do_plot();
#'
#' @param algo_0 A component algorithm to be switched to when e == 0 (AlgoNode)
#' @param algo_1 A component algorithm to be switched to when e == 1 (AlgoNode)
#' @param ... For future usage
#' @return A component algorithm whose inner logic switches algo_0 and algo_1 based on e
#' @export
do_design_composite_switch <- function(
  algo_0,
  algo_1,
  ...){

  if(algo_0$get_input_dimension() != algo_1$get_input_dimension()) { stop("Input dimensions are not identical"); };
  if(algo_0$get_output_dimension() != algo_1$get_output_dimension()) { stop("Output dimensions are not identical"); };

  input_dimension <- algo_0$get_input_dimension() + 1;
  output_dimension <- algo_0$get_output_dimension();
  extra_bit <- paste0("i", input_dimension);

  algo_switch <- AlgoComposite$new(input_dimension, output_dimension);

  algo_switch$set_inner_node(algo_0);
  algo_switch$set_inner_node(algo_1);

  # Loop on the component algo bits
  for(position in 1 : (input_dimension - 1)){
    # Pipes switch input bits to component bits.
    # Like this, both component algorithms will be unconditionally executed.
    algo_switch$set_inner_edge(algo_switch, paste0("i", position), algo_0, paste0("i", position));
    algo_switch$set_inner_edge(algo_switch, paste0("i", position), algo_1, paste0("i", position));
  }

  # Loop on the component algo bits
  for(position in 1 : output_dimension){

    # If extra_bit == 0, keep algo_0 output bit. Otherwise, set it to 0.
    sub_switch_0 <- algo_0010$new();
    algo_switch$set_inner_node(sub_switch_0);
    algo_switch$set_inner_edge(algo_0, paste0("o", position), sub_switch_0, "i1");
    algo_switch$set_inner_edge(algo_switch, extra_bit, sub_switch_0, "i2");

    # If extra_bit == 1, keep algo_1 output bit. Otherwise, set it to 0.
    sub_switch_1 <- AlgoAND$new();
    algo_switch$set_inner_node(sub_switch_1);
    algo_switch$set_inner_edge(algo_1, paste0("o", position), sub_switch_1, "i1");
    algo_switch$set_inner_edge(algo_switch, extra_bit, sub_switch_1, "i2");

    # OR the two switches
    sub_or_1 <- AlgoOR$new();
    algo_switch$set_inner_node(sub_or_1);
    algo_switch$set_inner_edge(sub_switch_0, "o1", sub_or_1, "i1");
    algo_switch$set_inner_edge(sub_switch_1, "o1", sub_or_1, "i2");

    # Pipe the OR to the final output bit
    algo_switch$set_inner_edge(sub_or_1, "o1", algo_switch, paste0("o", position));

  }

  return(algo_switch);

}
