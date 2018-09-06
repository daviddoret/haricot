convert_position_to_logical_vector <- function(position, size){
  # converter function
  i <- convert_position_to_int(position);
  logical_vector <- convert_int_to_logical_vector(i, size = size);
  return(logical_vector);
}
