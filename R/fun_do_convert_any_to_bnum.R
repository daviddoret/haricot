#' do_convert_any_to_bnum
#'
#' @description Convert a modular binary number represented in an arbitrary type to a modular binary number typed as bnum.
#'
#' @examples do_convert_any_to_bnum("1101");
#' do_convert_any_to_bnum(c(TRUE,TRUE,FALSE,TRUE));
#' do_convert_any_to_bnum(bnum$new("1101"));
#'
#' @param input A modular binary number (logical vector, character vector, bnum)
#' @return A modular binary number (bnum)
#' @export
do_convert_any_to_bnum <- function(input){
  if(is(object = input, class2 = "character")){
    return(do_convert_character_to_bnum(input));
  } else if(is(object = input, class2 = "logical")){
    return(do_convert_logical_to_bnum(input));
  } else if(is(object = input, class2 = "bnum")) {
    return(input$copy());
  } else {
    stop("Class not supported");
  }
}
