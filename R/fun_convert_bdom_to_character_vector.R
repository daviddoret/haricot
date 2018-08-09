#' convert_bdom_to_character_vector
#'
#' @description Takes a binary domain a returns a vector of character representations.
#'
#' @examples # R function style:
#' b1 <- bdom$new(dimension = 3);
#' convert_bdom_to_character_vector(b1);
#'
#' # R6 method style:
#' b1$convert_to_character_vector();
#'
#' @param binary_domain A binary domain (R6 Class bdom)
#' @param ... For future usage
#' @return A vector where each item correspond to a binary number of the binary domain (character vector)
#' @export
convert_bdom_to_character_vector = function(binary_domain, ...) {
  return(
    apply(
      X = binary_domain$get_logical_matrix(),
      MARGIN = 1,
      FUN = convert_logical_vector_to_character));
}
