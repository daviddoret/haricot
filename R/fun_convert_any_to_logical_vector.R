require(futile.logger);
#' convert_any_to_logical_vector
#'
#' Receives an input of an arbitrary supported class and returns a binary number represented by a logical vector.
#' NULL is converted to a logical vector of length 1 with a FALSE value.
#'
#' @examples # R function style:
#' convert_any_to_logical_vector("10001");
#' convert_any_to_logical_vector(c(TRUE,FALSE,FALSE,FALSE,TRUE));
#' convert_any_to_logical_vector(17);
#'
#' @param input An object or scalar (any supported class)
#' @param dim (conditional) A fixed dimension for the resulting binary number (integer)
#' @param ...
#' @return The resulting binary number represented as a logical vector (logical vector)
#' @export
convert_any_to_logical_vector <- function(input = NULL, dim = NULL, ...){
  if(is.null(input)){
    output <- c(FALSE);
  } else if(is(object = input, class2 = "logical")){
    output <- input;
  } else if(is(object = input, class2 = "character")){
    output <- convert_character_to_logical_vector(input);
  } else if(is(object = input, class2 = "bnum")) {
    output <- input$get_logical_vector();
  } else {
    flog.error("convert_any_to_logical_vector: input is not of a supported class");
    stop(input);
  };
  if(is.null(dim)){
    return(output);
  } else {
    return(output[1:dim]);
  };
};
