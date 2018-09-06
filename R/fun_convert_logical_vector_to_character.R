convert_logical_vector_to_character <- function(logical_vector){
  # Receives a logical vector and returns a string composed of "0" and "1".
  numeric_vector <- as.integer(logical_vector);
  character_vector <- as.character(numeric_vector);
  collapsed <- paste(character_vector, collapse = "");
  return(collapsed);
}
