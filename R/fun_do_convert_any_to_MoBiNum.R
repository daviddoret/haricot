#' do_convert_any_to_MoBiNum
#'
#' @description Convert a modular binary number represented in an arbitrary type to a modular binary number typed as MoBiNum.
#'
#' @examples do_convert_any_to_MoBiNum("1101");
#' do_convert_any_to_MoBiNum(c(TRUE,TRUE,FALSE,TRUE));
#' do_convert_any_to_MoBiNum(MoBiNum$new("1101"));
#'
#' @param input A modular binary number (logical vector, character vector, MoBiNum)
#' @return A modular binary number (MoBiNum)
#' @export
do_convert_any_to_MoBiNum <- function(input){
  if(is(object = input, class2 = "character")){
    return(do_convert_character_to_MoBiNum(input));
  } else if(is(object = input, class2 = "logical")){
    return(do_convert_logical_to_MoBiNum(input));
  } else if(is(object = input, class2 = "MoBiNum")) {
    return(input$copy());
  } else {
    stop("Class not supported");
  }
}
