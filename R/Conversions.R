convert_int_to_logical_vector <- function(i, size){
  # TODO: Check that size is enough to store i, otherwise do something, maybe modulo.
  return(as.logical(intToBits(i))[seq.int(from=1,to=size)]);
}

convert_int_to_position <- function(i){
  # converter function
  # Integers start from 0
  # but vector index positions start from 1
  return(i + 1);
}
convert_position_to_int <- function(position){
  # converter function
  # Vector position start from 1
  # Integer start from 0
  return(position - 1);
}
convert_logical_vector_to_position <- function(logical_vector){
  # converter function
  return(convert_int_to_position(convert_logical_vector_to_int(logical_vector)));
}
convert_position_to_logical_vector <- function(position, size){
  # converter function
  i <- convert_position_to_int(position);
  logical_vector <- convert_int_to_logical_vector(i, size = size);
  return(logical_vector);
}

convert_logical_vector_to_int <- function(logical_vector){
  # TODO: Check that size of bit vector is correct.
  size <- length(logical_vector);
  int_vector <- 2 ^ seq.int(from=0, to=size - 1) * logical_vector;
  output <- sum(int_vector);
  return(output);
}

convert_character_to_logical_vector <- function(input_character){
  # Receives a character string composed of "0" and "1" and converts it to logical vector.
  numeric_vector <- as.numeric(strsplit(x = input_character, split = "", fixed = TRUE)[[1]]);
  logical_vector <- as.logical(numeric_vector);
  return(logical_vector);
}

convert_logical_vector_to_character <- function(logical_vector){
  # Receives a logical vector and returns a string composed of "0" and "1".
  numeric_vector <- as.numeric(logical_vector);
  character_vector <- as.character(numeric_vector);
  collapsed <- paste(character_vector, collapse = "");
  return(collapsed);
}

convert_any_to_logical_vector <- function(input){
  # Receives an input of an arbitrary support class and returns a logical vector.
  if(is(object = input, class2 = "logical")){
    return(input);
  } else if(is(object = input, class2 = "character")){
    return(convert_character_to_logical_vector(input));
  } else if(is(object = input, class2 = "BinaryNumber_Modular")) {
    return(input$get_logical_vector());
  } else {
    # Ooops!
    stop(input);
  }
}

# Convert a NandTree to a TruthTable.
# The algorithm is inefficient,
# it simply loops through all input values,
# to build the truth table one item at a time.
convert_nandtree_to_truthtable <- function(nandtree){
  input_dimension <- nandtree$get_input_dimension();
  output_dimension <- nandtree$get_output_dimension();
  truthtable <- TruthTable_FlexOutput$new(input_dimension = input_dimension,
                                          output_dimension = output_dimension);
  input_binarynumber <- BinaryNumber_Modular$new(input = rep(FALSE, input_dimension));
  repeat{
    print(input_binarynumber);
    output_binarynumber = nandtree$do_apply_algorithm(input_binarynumber);
    print(output_binarynumber);
    truthtable$set_output(input = input_binarynumber, output_binarynumber);

    input_binarynumber$do_increment();
    if(input_binarynumber$get_equal_0()){
      break;
    }
  }

  return(truthtable);

}

