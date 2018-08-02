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

  # UNCERTAINTY: g is a copy of the original graph,
  # hence we may work with its push_execution_*** attributes freely.
  # This is important to support parallel algo execution.
  # TODO: Confirm this.

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, algo$get_output_dimension());

  push_execution <- function(
    vertex_name,
    push_execution_value,
    target_position = NULL){

    vertex <- V(g)[V(g)$name == vertex_name];
    print(paste0("node_id: ", vertex$node_id));

    # InputBit.
    if(vertex$type == "inputbit"){
      bit_id <- vertex$bit_id;
      position <- substr(bit_id, 2, nchar(bit_id));
      vertex$push_execution_value[target_position] <- push_execution_value;
      # Push its value to the successor vertices.
      next_vertices <- neighbors(graph = g, v = vertex, mode = "out");
      if(length(next_vertices) > 0){
      for(next_vertex_name in next_vertices$name){
        push_execution(
          vertex_name = next_vertex_name,
          push_execution_value = push_execution_value,
          target_position = position);
      }}
    }

    # Algo.
    if(vertex$type == "algo"){
      vertex$push_execution_value[target_position] <- push_execution_value;

      # Check if all InputBits have their value.
      if(!any(is.na(vertex$push_execution_value))){
        # If yes, execute the algo.
        node_id <- vertex$node_id;
        node <- algo$get_inner_node(node_id);
        algo_output <- node$do_execute(vertex$push_execution_value);
        # Push the algo result to the OutputBit vertices.
        next_vertices <- neighbors(graph = g, v = vertex, mode = "out");
        for(next_vertex in next_vertices){
          next_vertex_name <- next_vertex$name;
          next_vertex_bit_id <- next_vertex$bit_id;
          next_vertex_bit_position <- substr(next_vertex_bit_id, 2, nchar(next_vertex_bit_id));

          push_execution(
            vertex_name = next_vertex_name,
            push_execution_value = push_execution_value[next_vertex_bit_position],
            target_position = 1);
        }
      } else {
        # If not, stop here.
      }
    }

    # OutpuBit.
    if(vertex$type == "outputbit"){
      vertex$push_execution_value[target_position] <- push_execution_value;
      # Push its value to the successor vertices.
      next_vertices <- neighbors(graph = g, v = vertex, mode = "out");
      for(next_vertex in next_vertices){
        next_vertex_name <- next_vertex$name;
        push_execution(
          vertex_name = next_vertex_name,
          push_execution_value = push_execution_value,
          target_position = 1);
      }
    }
  }

  for(bit_position in 1:algo$get_input_dimension()){
    bit_id <- paste0("i", bit_position);
    node_id <- algo$get_node_id();
    vertex_name <- paste0(node_id, ".", bit_id);
    push_execution(
      vertex_name,
      push_execution_value = input_logical_vector[bit_position],
      target_position = 1);
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
