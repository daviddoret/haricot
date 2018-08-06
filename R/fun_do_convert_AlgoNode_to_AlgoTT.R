#' do_convert_AlgoNode_to_AlgoTT
#'
#' @description This function builds a truth table from an arbitrary algorithm
#' by executing it with all possible inputs.
#' Of course, this function is highly inefficient
#' and may only be applied to low input dimensional algorithms.
#'
#' @examples # R function style:
#' a1 <- AlgoNOT$new();
#' a2 <- do_convert_AlgoNode_to_AlgoTT(a1);
#' print(a2);
#'
#' # R6 method style:
#' a3 <- a1$convert_to_AlgoTT();
#' print(a3);
#'
#' @param algo An arbitrary algorithm (R6 Class AlgoNode)
#' @return The equivalent truth-table-based algorithm (R6 Class AlgoTT)
#' @export
do_convert_AlgoNode_to_AlgoTT <- function(algo){
  input_dimension <- algo$get_input_dimension();
  output_dimension <- algo$get_output_dimension();
  tt <- AlgoTT$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);
  input_binarynumber <- MoBiNum$new(input = rep(FALSE, input_dimension));

  repeat{
    output_binarynumber = algo$do_execute(input_binarynumber);
    tt$set_output(input = input_binarynumber, output_binarynumber);

    input_binarynumber$do_increment();
    if(input_binarynumber$get_equal_0()){
      break;
    }
  }

  return(tt);
}
