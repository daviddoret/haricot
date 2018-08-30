#' Function: concat_bnum
#'
#' @description Concatenates modular binary numbers.
#'
#' @section References:
#' \itemize{
#' \item{Concatenation of modular binary numbers (\link{concatenation_modular_binary_numbers})}
#' }
#'
#' @examples Function style:
#' n1 <- bnum$new("0101");
#' n2 <- bnum("11000");
#' n3 <- concat_bnum(n1, n2);
#' print(n3);
#'
#' R6 method style:
#' n4 <- n1$concat_with(n2);
#' print(n4);
#'
#' Infix operator style:
#' n5 <- n1 %concat% n2;
#' print(n5);
#'
#' @param ... Modular binary numbers (bnum)
#' @return The result of the concatenation
#' @family concatenations
#' @name concat_bnum
#' @export
concat_bnum <- function(...){

  dots <- list(...);
  if(length(dots) == 0) { stop("No bnum provided"); };
  if(length(dots) > 2) { stop("oops, not yet implemented, sorry"); };

  a1 <- dots[[1]];
  b1 <- dots[[2]];

  c1_char <- paste0(a1$convert_to_character(), b1$convert_to_character());
  c1 <- bnum$new(c1_char);
  return(c1);
}

`%concat%` <- function(a,b){
  return(concat_bnum(a,b));
}
