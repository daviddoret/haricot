#' equal_bnum
#'
#' @description Tells us if two bnum objects are equal.
#'
#' @examples equal_bnum(bnum$new("1101"), bnum$new("110"));
#' equal_bnum(bnum$new("1101"), bnum$new("1100"));
#' equal_bnum(bnum$new("1101"), bnum$new("1101"));
#'
#' @param n1 A modular binary number (logical vector, character vector, bnum)
#' @param n2 A modular binary number (logical vector, character vector, bnum)
#' @return Returns TRUE if n1 and n2 are equal, FALSE otherwise (logical vector of length 1)
#' @rdname man_bnum_equality
#' @export
equal_bnum <- function(n1, n2){
  n1_binum <- convert_any_to_bnum(n1);
  n2_binum <- convert_any_to_bnum(n2);
  output <- FALSE;
  if(n1_binum$get_dimension() == n2_binum$get_dimension()){
    if(all(n1_binum$get_logical_vector() == n2_binum$get_logical_vector())){
      output <- TRUE;
      }
    }
  return(output);
  }
