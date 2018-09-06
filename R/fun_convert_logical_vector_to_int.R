convert_logical_vector_to_int <- function(logical_vector){
  # TODO: Check that size of bit vector is correct.
  size <- length(logical_vector);
  int_vector <- 2 ^ seq.int(from=0, to=size - 1) * logical_vector;
  output <- sum(int_vector);
  return(output);
}
