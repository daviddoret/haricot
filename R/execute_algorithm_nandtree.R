#' Execute the algorithm of a NAND tree on a given input.
#'
#' @description A NAND tree, by definition, is an algorithm
#' that takes x bits as input,
#' applies to them a tree (or directed graph) of NAND functions,
#' and returns y bits as output.
#' This function applies / executes / runs
#' that algorithm on a given input
#' and returns the corresponding output.
#'
#' @usage # R function style:
#' execute_algorithm_nandtree(nandtree, input);
#'
#' # R6 method style:
#' nandtree$execute_algorithm(input);
#'
#' @param nandtree A NandTree (R6 Class NandTree)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
execute_algorithm_nandtree = function(nandtree, input) {
  # Applies the TruthTable algorithm and returns its output.
  # Returns a type that is consistent with the type of the input.
  input_logical_vector <- convert_any_to_logical_vector(input);

  # Prepare a vector of answers.
  # This will help us compute every subnode only once.
  subnode_answers <- rep(NA, nandtree$get_subnode_count());
  names(subnode_answers) <- nandtree$logical_datatable[,"subnode_id"];

  # Assign the correct answers to the input subnodes.
  for(input_subnode_number in 1:nandtree$get_input_dimension()){
    subnode_id <- paste0("i", input_subnode_number);
    subnode_answers[subnode_id] <- input_logical_vector[input_subnode_number];
  }

  do_solve_subnode <- function(subnode_id){
    # Check if this is a proper subnode.
    if(subnode_id == "NA"){
      warning("Attempt to solve NA");
      answer <- "NA";
    } else if(1 == 2) {
      # TODO: Add here logic to check if the subnode does not exist.
      answer <- "NA";
    } else {
      # Check if we already have a final value for this subnode.
      answer <- subnode_answers[subnode_id];
      if(!is.na(answer)){
        # This is a "cache hit".
        # message("Cache hit", subnode_id);
      } else {
        subnode <- nandtree$get_subnode_by_subnode_id(subnode_id);
        if(subnode$type == "i"){
          # It should have been a "cache hit",
          # the input value should have answered this subnode.
          stop("Input Node without value.", subnode);
        } else if (subnode$type == "n"){
          # This is a NAND Node.
          sub_answer_1 <- do_solve_subnode(subnode$param1_id);
          sub_answer_2 <- do_solve_subnode(subnode$param2_id);
          if(sub_answer_1 == "NA" | sub_answer_2 == "NA"){
            warning("NAND Node based on NA", subnode);
            answer <- "NA";
          } else {
            answer <- do_nand(sub_answer_1, sub_answer_2);
          }
        } else if (subnode$type == "o"){
          # This is a NAND Node.
          sub_answer_1 <- do_solve_subnode(subnode$param1_id);
          if(sub_answer_1 == "NA"){
            warning("Output Node based on NA", subnode);
            answer <- "NA";
          } else {
            answer <- sub_answer_1;
          }
        }
      }
    }
    # message("Node answer", subnode_id, answer);
    return(answer);
  }

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, nandtree$get_output_dimension());

  # Loop on all output subnodes.
  for(output_subnode_number in 1:nandtree$get_output_dimension()){
    subnode_id <- paste0("o", output_subnode_number);
    output_subnode_output <- do_solve_subnode(subnode_id);
    if(output_subnode_output == "NA"){
      # To avoid issues when rbinding tables,
      # we improperly used the string "NA".
      output_subnode_output <- NA;
    }
    output_logical_vector[output_subnode_number] <- output_subnode_output;
  }

  if(is(input, "logical")){
    return(output_logical_vector);
  } else if(is(input, "character")){
    return(convert_logical_vector_to_character(output_logical_vector));
  } else if(is(input, "BinaryNumber_Modular")){
    return(BinaryNumber_Modular$new(output_logical_vector));
  } else {
    # Oooops!
    stop(input);
  }
}
