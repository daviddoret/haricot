#' convert_int_to_bnum
#'
#' Convert a native R integer value into a logical vector representation.
#'
#' @examples # R function style:
#' convert_int_to_bnum(57, 8);
#'
#' @param i An integer (integer)
#' @param dim The dimension of the binary number (integer)
#' @param ... For future use
#' @return The resulting binary number (R6 class bnum)
#' @export
convert_int_to_bnum <- function(i = NULL, dim = NULL, ...){
  if(is.null(i)){ i <- 0; };
  i_logical_vector <- convert_int_to_logical_vector(i, dim, ...);
  i_bnum <- bnum$new(i_logical_vector, dim, ...);
  return(i_bnum);
}
