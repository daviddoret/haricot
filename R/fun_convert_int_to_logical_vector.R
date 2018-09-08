require(futile.logger);
#' convert_int_to_logical_vector
#'
#' Convert a native R integer value into a binary number materialized by an R logical vector. \cr
#' See \code{\link[modular_binary_number]{modular binary number}} for a definition of binary number. \cr
#' See \code{\link{convert_any_to_logical_vector}} for the type generic equivalent function. \cr
#' Open question: should this function provide support for vectors of integers? If yes, what should it return? A matrix? \cr
#'
#' @examples # R function style:
#' convert_int_to_logical_vector(137, 9);
#'
#' @param i A natural number (integer)
#' @param dim (conditional) The size (length) of the logical vector (integer)
#' @return The logical vector (logical vector)
#' @export
convert_int_to_logical_vector <- function(i = NULL, dim = NULL, ...){
  # Default NULL to 0.
  if(is.null(i)) {
    flog.info("convert_int_to_logical_vector: i was NULL, default to 0");
    i <- 0;
  };
  if(!is.integer(i)){
    flog.error("convert_int_to_logical_vector: i is not integer");
    stop("");
  };
  if(i < 0){
    flog.error("convert_int_to_logical_vector: i is negative");
    stop("");
  };

  i_logical <- NULL;
  if(i == 0){
    i_logical <- c(FALSE);
  } else {
    # intToBits returns a fixed size logical vector.
    i_logical <- as.logical(intToBits(i));
    # Find the position of the last meaningful bit in the sequence.
    last_pos <- max(which(i_logical,TRUE));
    # Truncate the result to only containt the meaningful bits, i.e. remove the leading zeroes.
    i_logical <- i_logical[1 : last_pos];
  };
  if(is.null(dim)) {
    dim <- length(i_logical);
  };
  if(length(i_logical) < dim){
    # Pad with zeroes.
    i_logical <- c(i_logical, rep(FALSE, dim - length(i_logical)));
  };
  return(i_logical[1 : dim]);
};
