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
  #input_character <- convert_logical_vector_to_character(input_logical_vector);

  # Get a copy of the igraph to store execution values.
  g <- algo$get_inner_graph();

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, algo$get_output_dimension());

  execute_vertex <- function(vertex_name, phushed_value){
    # Execute whatever can be executed.

    vertex <- V(g)[V(g)$name == vertex_name];

    print(paste0("node_id: ", vertex$node_id));

    # InputBit.
    if(type == "inputbit"){
      vertex$exec_value <- pushed_value;
      # Push its value to the successor vertices.
      next_vertex <- neighbors(graph = g, v = vertex, mode = "out");
      for(sub_vertex in next_vertex){
        # TODO
      }
    }

    # Algo.
    if(type == "algo"){
      # Check if all InputBits have their value.
      # If not, stop here.
      # If yes, execute the algo and push the result to its OutputBits.
    }

    # OutpuBit.
    if(type == "outputbit"){
      V(g)[vertex_filter]$exec_value <- pushed_value;
      # Push its value to the successor vertices.
      # TODO
    }
  }

  for(bit_position in 1:algo$get_input_dimension()){
    bit_id <- paste0("i", bit_position);
    node_id <- algo$get_node_id();
    vertex_name <- paste0(node_id, ".", bit_id);
    execute_vertex(vertex_name, pushed_value = input[bit_position]);
  }

  # Retrieve the result of the algo from the parent OutputBits.
  # TODO

  # Return the output in the requested type.
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
