#' Function: do_concatenate_MoBiNum
#'
#' @description Concatenates modular binary numbers.
#'
#' @section Definitions:
#' \itemize{
#' \item{Concatenation of modular binary numbers (\link{def_concatenation_modular_binary_numbers})}
#' }
#'
#' @examples Function style:
#' n1 <- MoBiNum$new("0101");
#' n2 <- MoBiNum("11000");
#' n3 <- do_concatenate_MoBiNum(n1, n2);
#' print(n3);
#'
#' R6 method style:
#' n4 <- n1$concatenate_with(n2);
#' print(n4);
#'
#' Infix operator style:
#' n5 <- n1 %&% n2;
#' print(n5);
#'
#' @param ... Modular binary numbers (MoBiNum)
#' @return The result of the concatenation
#' @family concatenations
#' @name do_concatenate_MoBiNum
#' @export
do_concatenate_MoBiNum <- function(...){

  dots <- list(...);
  if(length(dots) == 0) { stop("No MoBiNum provided"); };
  if(length(dots) > 2) { stop("oops, not yet implemented, sorry"); };

  a1 <- dots[[1]];
  b1 <- dots[[2]];

  c1_char <- paste0(a1$convert_to_character(), b1$convert_to_character());
  c1 <- MoBiNum$new(c1_char);
  return(c1);
}

`%&%` <- function(a,b){
  return(do_concatenate_MoBiNum(a,b));
}
