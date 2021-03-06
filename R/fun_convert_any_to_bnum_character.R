#' convert_any_to_bnum_character
#'
#' Convert a modular binary number of an arbitrary type to a modular binary number typed as character.
#'
#' @description A modular binary number typed as a character is a string composed of "0"s and "1"s.
#' The leftmost character (position = 1) being the least significant bit.
#'
#' @examples # R function style:
#' convert_any_to_bnum_character(input);
#'
#' # R6 method style:
#' modbinum$convert_to_character();
#'
#' @param input A modular binary number (logical vector, character vector, BiNum)
#' @return A modular binary number (character)
#' @export
convert_any_to_bnum_character <- function(input){
  if(is(object = input, class2 = "character")){
    return(input);
  } else if(is(object = input, class2 = "logical")){
    return(convert_logical_vector_to_character(input));
  } else if(is(object = input, class2 = "bnum")) {
    return(input$convert_to_character());
  } else {
    stop("Class not supported");
  }
}
