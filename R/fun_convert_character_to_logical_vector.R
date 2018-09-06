convert_character_to_logical_vector <- function(input_character){
  # Receives a character string composed of "0" and "1" and converts it to logical vector.
  numeric_vector <- as.numeric(strsplit(x = input_character, split = "", fixed = TRUE)[[1]]);
  logical_vector <- as.logical(numeric_vector);
  return(logical_vector);
}
