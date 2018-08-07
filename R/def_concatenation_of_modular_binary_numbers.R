#' @title Definition: Concatenation of modular binary numbers
#'
#' @section Definition:
#' In the context of this package, the concatenation operation applied on two modular binary numbers \eqn{a} and \eqn{b} means:
#' \enumerate{
#' \item Create a new modular binary number \eqn{c} of dimension \eqn{|a| + |b|},
#' \item Set the value of the \eqn{|a|} least significant (leftmost) bits of \eqn{c} to the value of the corresponding bits of \eqn{a},
#' \item Set the value of the \eqn{|b|} remaining bits of \eqn{c} to the value of the corresponding bits of \eqn{b}.
#' }
#'
#' @section Notation:
#' The concatenation operator is noted:\eqn{&}.
#'
#' @section Samples:
#' \deqn{ 01 & 1010 = 011010 }
#' \deqn{ 000 & 00 = 00000 }
#'
#' @name def_concatenation_modular_binary_numbers
#' @family concatenations
NULL;
