#' split_algo_tt
#'
#' Decompose a truth table algorithm into two sub-truth table algorithm with a switch.
#'
#' @examples ...
#'
#' @param algo ... (algo_tt)
#' @param ... For future usage
#' @return A composite algorithm that has an identitcal truth table to the input algorithm (algo_composite)
#' @export
split_algo_tt <- function(
  algo,
  ...){

  # TODO: Check correct class of input.

  # Retrieve the dimensions of the original algo.
  dim_i_original <- algo$get_input_dimension();
  dim_o_original <- algo$get_output_dimension();

  dim_i_sub <- dim_i_original - 1;
  dim_o_sub <- dim_o_original;

  # Retrieve the raw logical matrix from the algo
  tt_raw <- algo$get_logical_matrix();

  # Compute the positions of the rows where to split the tt in half horizontally.
  split_0_last_position <- nrow(tt_raw) / 2;
  split_1_first_position <- split_0_last_position + 1;

  # Split horizontally the tt in two equal parts.
  tt_0 <- tt_raw[1 : split_0_last_position,];
  tt_1 <- tt_raw[split_1_first_position : nrow(tt_raw),];

  # Prepares the new sub algorithms.
  algo_0 <- algo_tt$new(input_dimension = dim_i_sub, output_dimension = dim_o_sub);
  algo_0$set_logical_matrix(tt_0);
  algo_1 <- algo_tt$new(input_dimension = dim_i_sub, output_dimension = dim_o_sub);
  algo_1$set_logical_matrix(tt_1);

  # Design the parent switch.
  algo_comp <- switch_algo(algo_0, algo_1);

}
