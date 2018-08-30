#' Transformation: truth tables to DAG
#'
#' I'll provide more details as soon as I have a moment to document this. \cr
#' But haricot has now a logically neutral transformation,
#' that takes an arbitrary truth table algorithm as input,
#' and transforms it into a DAG algorithm.}
#'
#' @examples
#' # Create a random truth table algorithm with input dimension 8 and output dimension 8:
#' truthtable <- algo_tt$new(8,8)$do_randomize_outputs();
#' # Display the upper part of the truth table in the console:
#' cat(substr(truthtable$get_prettystring(),1,200));
#' # Transform the truth table to a DAG algorithm:
#' dag <- atomize(truthtable);
#' #
#'
#' @name transform_truthtables_into_dag
NULL;
