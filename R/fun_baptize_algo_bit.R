#' baptize_algo_bit
#'
#' @description Assign a proper name for a bit in an algo.
#' The naming scheme goes like this:
#' \itemize{
#' \item{For input bits: \code{"i1", "i2", "i3", ...}}
#' \item{For output bits: \code{"o1", "o2", "o3", ...}}
#' \item{For non-bits (e.g. the "bit" attribute of an algo vertex in an igraph): \code{"x"} (empty string)}
#' }
#' Any programmatic manipulation of bits should avoid to implement bit names directly and rather rely on this function to guarantee upward compatibility with future evolutions of the naming scheme.
#'
#' @examples print(baptize_algo_bit("i", 5));
#' print(baptize_algo_bit("o", 1:4));
#'
#' @param type The bit type: "i" for input or "o" for output (character)
#' @param number The bit number (integer)
#' @param ... For future usage
#' @return The bit name
#' @export
baptize_algo_bit <- function(
  type,
  number,
  ...){

  if(is_missing(type) | is.null(type)){ stop("missing type"); };
  if(type != "i" & type != "x" & type != "o"){ stop("invalid type"); };
  if(is_missing(number) | is.null(number)){ stop("missing number"); };

  return(paste0(type, number));

}
