convert_logical_vector_to_position <- function(logical_vector){
  # converter function
  return(convert_int_to_position(convert_logical_vector_to_int(logical_vector)));
}
