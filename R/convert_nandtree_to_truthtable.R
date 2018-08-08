#' Convert a NAND tree to a truth table.
#'
#' @description This function builds a truth table from a NAND tree
#' by executing its algorithm with all possible inputs.
#' Of course, this function is highly inefficient
#' and may only be applied to low input dimensional NAND trees.
#'
#' @examples # R function style:
#' convert_nandtree_to_truthtable(nandtree);
#'
#' # R6 method style:
#' nandtree$convert_to_truthtable();
#'
#' @param nandtree A NandTree (R6 Class NandTree)
#' @return The resulting truth table (R6 Class Truthtable)
#' @export
convert_nandtree_to_truthtable <- function(nandtree){
  input_dimension <- nandtree$get_input_dimension();
  output_dimension <- nandtree$get_output_dimension();
  truthtable <- AlgoTT$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);
  input_binarynumber <- bnum$new(input = rep(FALSE, input_dimension));
  repeat{
    print(input_binarynumber);
    output_binarynumber = nandtree$do_execute(input_binarynumber);
    print(output_binarynumber);
    truthtable$set_output(input = input_binarynumber, output_binarynumber);

    input_binarynumber$do_increment();
    if(input_binarynumber$get_equal_0()){
      break;
    }
  }
  return(truthtable);
}
