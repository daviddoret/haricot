#' baptize_algo_bit
#'
#' @description Assign a proper name for a bit in an algo.
#' The naming scheme goes like this:
#' \itemize{
#' \item{For input bits: \code{"i1", "i2", "i3", ...}}
#' \item{For output bits: \code{"o1", "o2", "o3", ...}}
#' \item{For non-bits (e.g. the "bit" attribute of an algo vertex in an igraph): \code{NOBIT_PREFIX} (empty string)}
#' }
#' Any programmatic manipulation of bits should avoid to implement bit names directly and rather rely on this function to guarantee upward compatibility with future evolutions of the naming scheme.
#'
#' @examples print(baptize_algo_bit(INPUT_PREFIX, 5));
#' print(baptize_algo_bit(OUTPUT_PREFIX, 1:4));
#'
#' @param type The bit type: INPUT_PREFIX for input or OUTPUT_PREFIX for output (character)
#' @param number The bit number (integer)
#' @param ... For future usage
#' @return The bit name
#' @export
baptize_algo_bit <- function(
  type,
  number = NULL,
  ...){

  if(missing(type) | is.null(type)){ stop("missing type"); };
  if(type != INPUT_PREFIX & type != NOBIT_PREFIX & type != OUTPUT_PREFIX){ stop("invalid type"); };
  if(type != NOBIT_PREFIX & (missing(number) | is.null(number))){ stop("missing number"); };
  if(type == NOBIT_PREFIX) { number <- ""; };
  return(paste0(type, number));

}
