#' Concatenates two truthtables of equal input dimension
#'
#' By concatenation in this context,
#' we mean merging together two algorithms/functions,
#' in such a way that the output of the merge algorithm/function
#' is equal to the output of the first truthtable
#' followed by the output of the second truthtable.
#' The two truthtables must have an identical input dimension.
#' @param truthtable_1 The first truthtable of size x (TruthTable)
#' @param truthtable_2 The second truthtable of size x (TruthTable)
#' @return A truthtable of size x (TruthTable)
#' @export
concatenate_truthtable <- function(truthtable_1, truthtable_2){

  logical_matrix_1 <- truthtable_1$get_logical_matrix();
  logical_matrix_2 <- truthtable_2$get_logical_matrix();

  if(truthtable_1$get_dim_i() !=
     truthtable_2$get_dim_i()){
    stop("Input dimensions are not equal", truthtable_1, truthtable_2);
  }

  logical_matrix_merged <- cbind(logical_matrix_1, logical_matrix_2);

  truthtable_merged <- algo_tt$new(
    dim_i = truthtable_1$get_dim_i(),
    dim_o =
      truthtable_1$get_dim_o() +
      truthtable_2$get_dim_o());

  truthtable_merged$set_logical_matrix(logical_matrix_merged);

  return(truthtable_merged);
}

