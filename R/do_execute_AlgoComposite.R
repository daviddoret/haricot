#' Execute the algorithm of a AlgoComposite on a given input.
#'
#' @description A AlgoComposite, by definition in this context, is an algorithm
#' that takes x bits as input,
#' applies to them a directed graph set of abstract node functions,
#' and returns y bits as output.
#' This function applies / executes / runs
#' that algorithm on a given input
#' and returns the corresponding output.
#'
#' @usage # R function style:
#' execute_algorithm_AlgoComposite(algo, input);
#'
#' # R6 method style:
#' algo$execute_algorithm(input);
#'
#' @param algo A composite algorithm (R6 Class AlgoComposite)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
do_execute_AlgoComposite = function(algo, input) {
  # Applies the TruthTable algorithm and returns its output.
  # Returns a type that is consistent with the type of the input.
  input_logical_vector <- convert_any_to_logical_vector(input);

  # Prepare a copy of the TODO: RESUME FROM HERE

  # Prepare lists of inputs and outputs for inner nodes.
  # This will help us compute every inner_node only once.
  inner_nodes_inputs <- list();
  inner_nodes_outputs <- list();


  # Assign the correct answers to the input inner_nodes.
  for(input_inner_node_number in 1:algo$get_input_dimension()){
    inner_node_id <- paste0("i", input_inner_node_number);
    inner_node_answers[inner_node_id] <- input_logical_vector[input_inner_node_number];
  }

  do_solve_inner_node <- function(inner_node_id){
    # Check if this is a proper inner_node.
    if(inner_node_id == "NA"){
      warning("Attempt to solve NA");
      answer <- "NA";
    } else if(1 == 2) {
      # TODO: Add here logic to check if the inner_node does not exist.
      answer <- "NA";
    } else {
      # Check if we already have a final value for this inner_node.
      answer <- inner_node_answers[inner_node_id];
      if(!is.na(answer)){
        # This is a "cache hit".
        # message("Cache hit", inner_node_id);
      } else {
        inner_node <- algo$get_inner_node_by_inner_node_id(inner_node_id);
        if(inner_node$type == "i"){
          # It should have been a "cache hit",
          # the input value should have answered this inner_node.
          stop("Input Node without value.", inner_node);
        } else if (inner_node$type == "n"){
          # This is a NAND Node.
          sub_answer_1 <- do_solve_inner_node(inner_node$param1_id);
          sub_answer_2 <- do_solve_inner_node(inner_node$param2_id);
          if(sub_answer_1 == "NA" | sub_answer_2 == "NA"){
            warning("NAND Node based on NA", inner_node);
            answer <- "NA";
          } else {
            answer <- do_nand(sub_answer_1, sub_answer_2);
          }
        } else if (inner_node$type == "o"){
          # This is a NAND Node.
          sub_answer_1 <- do_solve_inner_node(inner_node$param1_id);
          if(sub_answer_1 == "NA"){
            warning("Output Node based on NA", inner_node);
            answer <- "NA";
          } else {
            answer <- sub_answer_1;
          }
        }
      }
    }
    # message("Node answer", inner_node_id, answer);
    return(answer);
  }

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, algo$get_output_dimension());

  # Loop on all output inner_nodes.
  for(output_inner_node_number in 1:algo$get_output_dimension()){
    inner_node_id <- paste0("o", output_inner_node_number);
    output_inner_node_output <- do_solve_inner_node(inner_node_id);
    if(output_inner_node_output == "NA"){
      # To avoid issues when rbinding tables,
      # we improperly used the string "NA".
      output_inner_node_output <- NA;
    }
    output_logical_vector[output_inner_node_number] <- output_inner_node_output;
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
