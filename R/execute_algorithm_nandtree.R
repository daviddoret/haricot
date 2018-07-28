
#' Execute the algorithm of a NAND tree on a given input.
#'
#' @description A NAND tree, by definition, is an algorithm
#' that takes x bits as input,
#' and returns y bits as output.
#' This function applies / executes / runs
#' that algorithm on a given input
#' and returns the corresponding output.
#'
#' @usage # R function
#' execute_algorithm_nandtree(nandtree, input);
#' \cr
#' # R6 method
#' nandtree$execute_algorithm(input);
#'
#' @param nandtree A NandTree (R6 Class TruthTable)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
execute_algorithm_nandtree = function(nandtree, input) {
  # Applies the TruthTable algorithm and returns its output.
  # Returns a type that is consistent with the type of the input.
  input_logical_vector <- convert_any_to_logical_vector(input);

  # Prepare a vector of answers.
  # This will help us compute every node only once.
  node_answers <- rep(NA, nandtree$get_node_count());
  names(node_answers) <- nandtree$logical_datatable[,"node_id"];

  # Assign the correct answers to the input nodes.
  for(input_node_number in 1:nandtree$get_input_dimension()){
    node_id <- paste0("i", input_node_number);
    node_answers[node_id] <- input_logical_vector[input_node_number];
  }

  do_solve_node <- function(node_id){
    # Check if this is a proper node.
    if(node_id == "NA"){
      warning("Attempt to solve NA");
      answer <- "NA";
    } else if(1 == 2) {
      # TODO: Add here logic to check if the node does not exist.
      answer <- "NA";
    } else {
      # Check if we already have a final value for this node.
      answer <- node_answers[node_id];
      if(!is.na(answer)){
        # This is a "cache hit".
        message("Cache hit", node_id);
      } else {
        node <- nandtree$get_node_by_node_id(node_id);
        if(node$type == "i"){
          # It should have been a "cache hit",
          # the input value should have answered this node.
          stop("Input Node without value.", node);
        } else if (node$type == "n"){
          # This is a NAND Node.
          sub_answer_1 <- do_solve_node(node$param1_id);
          sub_answer_2 <- do_solve_node(node$param2_id);
          if(sub_answer_1 == "NA" | sub_answer_2 == "NA"){
            warning("NAND Node based on NA", node);
            answer <- "NA";
          } else {
            answer <- do_nand(sub_answer_1, sub_answer_2);
          }
        } else if (node$type == "o"){
          # This is a NAND Node.
          sub_answer_1 <- do_solve_node(node$param1_id);
          if(sub_answer_1 == "NA"){
            warning("Output Node based on NA", node);
            answer <- "NA";
          } else {
            answer <- sub_answer_1;
          }
        }
      }
    }
    message("Node answer", node_id, answer);
    return(answer);
  }

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, nandtree$get_output_dimension());

  # Loop on all output nodes.
  for(output_node_number in 1:nandtree$get_output_dimension()){
    node_id <- paste0("o", output_node_number);
    output_node_output <- do_solve_node(node_id);
    if(output_node_output == "NA"){
      # To avoid issues when rbinding tables,
      # we improperly used the string "NA".
      output_node_output <- NA;
    }
    output_logical_vector[output_node_number] <- output_node_output;
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
