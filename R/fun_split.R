#' split
#'
#' Decompose a truth table algorithm into two sub-truth table algorithm with an arbitrage bit (positioned last) to switch between the two nicely packaged into a new composite algorithm.
#'
#' @section Transformation characteristics:
#' \itemize{
#' \item{\code{\link{dimensional_equivalence}}}
#' \item{\code{\link{logical_equivalence}}}
#' }
#'
#' @examples
#' # Pick a random input dimension
#' dim_i <- sample(x = 2:6, size = 1, replace = TRUE);
#' # Pick a random output dimension
#' dim_o <- sample(x = 2:6, size = 1, replace = TRUE);
#' # Create a truth table algorithm of desired dimensions
#' truthtable_algo <- algo_tt$new(input_dimension = dim_i, output_dimension = dim_o);
#' # Randomize the truth table outputs, we end up with a random deterministic algorithm
#' truthtable_algo$do_randomize_outputs();
#' # Split the random truth table algorithm and retrieve the resulting composite algorithm
#' splitted_algo <- split(truthtable_algo);
#' for(test_counter in 1:3){
#'    # Pick a random input value
#'    n <- bnum$new(dim = dim_i)$randomize();
#'    print(paste0("Input: ", n$format(), ", output of truth table algo: ", truthtable_algo$exec(n)$format(), ", output of splitted algo: ", splitted_algo$exec(n)$format()));
#' }
#' # Plot the original algo
#' truthtable_algo$plot();
#' # Plot the splitted algo
#' splitted_algo$plot();
#'
#' @param algo The truth table algorithm to be splitted (algo_tt)
#' @param ... For future usage
#' @return A composite algorithm that has an identical truth table to original truth table algorithm but where the truth table has been split in two and an arbitrage bit is used (algo_composite)
#' @family transformations
#' @name split
#' @export
split <- function(
  algo,
  ...){

  if(!is(algo, "algo_tt")) { stop("algo does not implement algo_tt"); };
  if(!algo$get_input_dimension() > 1){ stop("algo must have an input dimension strictly greater than 1."); };

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
