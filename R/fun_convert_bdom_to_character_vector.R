#' convert_bset_to_character_vector
#'
#' @description Takes a binary set a returns a vector of character representations.
#'
#' @examples # R function style:
#' b1 <- bset$new(dimension = 3);
#' convert_bset_to_character_vector(b1);
#'
#' # R6 method style:
#' b1$convert_to_character_vector();
#'
#' @param binary_set A binary set (R6 Class bset)
#' @param ... For future usage
#' @return A vector where each item correspond to a binary number of the binary set (character vector)
#' @export
convert_bset_to_character_vector = function(binary_set, ...) {
  return(
    apply(
      X = binary_set$get_logical_matrix(),
      MARGIN = 1,
      FUN = convert_logical_vector_to_character));
}
