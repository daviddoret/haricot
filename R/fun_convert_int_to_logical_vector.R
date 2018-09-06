#' Convert a native R integer value into a logical vector representation.
#'
#' In this package, we often use logical vectors to represent modulo integer values.
#' In this context, we use a binary representation where the least significant bit is on the left (at vector index position 1).
#' This function takes a native R integer value and converts it into such a logical vector.
#'
#' @examples # R function style:
#' convert_int_to_logical_vector(i, size);
#'
#' @param i An integer (integer)
#' @param dim (conditional) The size (length) of the logical vector (integer)
#' @return The logical vector (logical vector)
#' @export
convert_int_to_logical_vector <- function(i = NULL, dim = NULL, ...){
  # Default NULL to 0.
  if(is.null(i)) { i <- 0; };

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
