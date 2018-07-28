#' Convert a modular binary number of an arbitrary type to a modular binary number typed as character.
#'
#' @description A modular binary number typed as a character is a string composed of "0"s and "1"s.
#' The leftmost character (position = 1) being the least significant bit.
#'
#' @usage # R function style:
#' convert_modbinum_any_to_modbinum_character(input);
#'
#' # R6 method style:
#' modbinum$convert_to_character();
#'
#' @param input A modular binary number (logical vector, character vector, BiNum)
#' @return A modular binary number (character)
#' @export
convert_modbinum_any_to_modbinum_character <- function(input){
  # Receives an binary number input of an arbitrary support class and returns a character representation
  if(is(object = input, class2 = "character")){
    return(input);
  } else if(is(object = input, class2 = "logical")){
    return(convert_logical_vector_to_character(input));
  } else if(is(object = input, class2 = "BinaryNumber_Modular")) {
    return(input$convert_to_binum_character());
  } else {
    # Ooops!
    stop(input);
  }
}
