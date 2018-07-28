#' Convert a native R integer value into a logical vector representation.
#'
#' @description In this package, we often use logical vectors to represent modulo integer values.
#' In this context, we use a binary representation where the least significant bit is on the left (at vector index position 1).
#' This function takes a native R integer value and converts it into such a logical vector.
#'
#' @usage # R function style:
#' convert_int_to_logical_vector(i, size);
#'
#' @param i An integer (integer)
#' @param size The size (length) of the logical vector (integer)
#' @return The logical vector (logical)
#' @export
convert_int_to_logical_vector <- function(i, size){
  # TODO: Check that size is enough to store i, otherwise do something, maybe modulo.
  return(as.logical(intToBits(i))[seq.int(from=1,to=size)]);
}
