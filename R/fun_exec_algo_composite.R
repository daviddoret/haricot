#' Execute the algorithm of a algo_composite on a given input.
#'
#' @description A algo_composite, by definition in this context, is an algorithm
#' that takes x bits as input,
#' applies to them a directed graph set of abstract node functions,
#' and returns y bits as output.
#' This function applies / executes / runs
#' that algorithm on a given input
#' and returns the corresponding output.
#'
#' @examples # R function style:
#' execute_algorithm_algo_composite(algo, input);
#'
#' # R6 method style:
#' algo$execute_algorithm(input);
#'
#' @param algo A composite algorithm (R6 Class algo_composite)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
exec_algo_composite = function(algo, input) {
  # Applies the TruthTable algorithm and returns its output.
  # Returns a type that is consistent with the type of the input.
  input_logical_vector <- convert_any_to_logical_vector(input);

  # Get a copy of the igraph to store execution values.
  g <- algo$get_inner_graph();

  # Prepare the vertex values
  vertices_execution_value <- list();

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, algo$get_output_dimension());

  push_execution <- function(
    vertex_name,
    pushed_value,
    pusher_position){

    #cat(
    #  "\n\nPUSH EXEC ",
    #  "vertex_name: ", vertex_name,
    #  "push_exec_value: ", pushed_value,
    #  "pusher_position: ", pusher_position,
    #  sep = "\n");

    vertex <- V(g)[V(g)$name == vertex_name];
    label <- vertex$label;
    # print(paste0("label: ", label));
    algo_id <- vertex$algo_id;
    # print(paste0("algo_id: ", algo_id));
    vertex_type <- vertex$type;
    # print(paste0("vertex_type: ", vertex_type));
    node <- NULL;
    if(algo_id == algo$get_algo_id()){
      # The node we are working on is the parent node.
      node <- algo;
    } else {
      node <- algo$get_inner_node(algo_id);
    }
    node_input_dimension <- node$get_input_dimension();
    # print(paste0("node_input_dimension: ", node_input_dimension));

    # InputBit.
    if(vertex_type == "inputbit"){
      # print(paste0("type == inputbit"));

      bit <- vertex$bit;
      # print(paste0("bit: ", bit));
      position <- as.integer(substr(bit, 2, nchar(bit)));
      # print(paste0("position: ", position));

      vertices_execution_value[[vertex_name]] <<- pushed_value;
      # Push its value to the successor vertices.
      next_vertices <- neighbors(graph = g, v = vertex, mode = "out");
      if(length(next_vertices) > 0){
      for(next_vertex_name in next_vertices$name){
        push_execution(
          vertex_name = next_vertex_name,
          pushed_value = pushed_value,
          pusher_position = position);
      }}
    } else if(vertex_type == "algo"){
      # print(paste0("type == algo"));
      if(is.null(vertices_execution_value[[vertex_name]])){
        init_vector <- c(rep(NA, node_input_dimension));
        # print(paste0("init_vector: ", init_vector));
        vertices_execution_value[[vertex_name]] <<- init_vector;
      }
      vertices_execution_value[[vertex_name]][pusher_position] <<- pushed_value;

      # Check if all InputBits have their value.
      if(!any(is.na(vertices_execution_value[[vertex_name]]))){
        # print("ALGO COMPLETED");
        # If yes, execute the algo.
        algo_input <- vertices_execution_value[[vertex_name]];
        # print(paste0("algo_input", algo_input));
        algo_output <- node$exec(vertices_execution_value[[vertex_name]]);
        # print(paste0("algo_output: ", algo_output));
        # Push the algo result to the OutputBit vertices.
        next_vertices <- neighbors(graph = g, v = vertex, mode = "out");
        for(next_vertex_name in next_vertices$name){
          next_vertex <- next_vertices[next_vertices$name == next_vertex_name];
          # print(paste0("next_vertex_name", next_vertex_name));
          next_vertex_bit <- next_vertex$bit;
          next_vertex_bit_position <- as.integer(substr(next_vertex_bit, 2, nchar(next_vertex_bit)));
          next_pushed_value <- algo_output[next_vertex_bit_position];
          push_execution(
            vertex_name = next_vertex_name,
            pushed_value = next_pushed_value,
            pusher_position = next_vertex_bit_position);
        }
      } else {
        # If not, stop here.
      }
    } else if(vertex_type == "outputbit"){
      # print(paste0("OutputBit"));

      vertices_execution_value[[vertex_name]] <<- pushed_value;
      # Push its value to the successor vertices.
      next_vertices <- neighbors(graph = g, v = vertex, mode = "out");
      for(next_vertex_name in next_vertices$name){
        next_vertex <- next_vertices[next_vertices$name == next_vertex_name];
        push_execution(
          vertex_name = next_vertex_name,
          pushed_value = pushed_value,
          pusher_position = 1);
      }
    }
  }

  for(bit_position in 1:algo$get_input_dimension()){
    bit <- paste0("i", bit_position);
    algo_id <- algo$get_algo_id();
    vertex_name <- paste0(algo_id, ".", bit);
    pushed_value <- input_logical_vector[bit_position];
    # cat("\n\n\nSTAGE 1: EXECUTE INPUTBIT",
    #  "bit: ", bit,
    #  "algo_id", algo_id,
    #  "vertex_name", vertex_name,
    #  "pushed_value", pushed_value,
    #  "bit_position", bit_position,
    #  sep = "\n");
    push_execution(
      vertex_name = vertex_name,
      pushed_value = pushed_value,
      pusher_position = bit_position);
  }

  # Retrieve the result of the algo from the parent OutputBits.
  output_logical_vector <- rep(NA, algo$get_output_dimension());
  for(position in 1:algo$get_output_dimension()){
    bit <- paste0("o", position);
    # print(paste0("\n\n\n\nSTAGE 2: RETRIEVE FROM BitOutput: bit: ", bit));
    vertex_name <- paste0(algo$get_algo_id(), ".", bit);
    bit_exec_value <- vertices_execution_value[[vertex_name]];
    # print(paste0("bit_exec_value: ", bit_exec_value));
    output_logical_vector[position] <- bit_exec_value;
  }
  # print(output_logical_vector);

  # Return the output in the requested type.
  if(is(input, "logical")){
    return(output_logical_vector);
  } else if(is(input, "character")){
    return(convert_logical_vector_to_character(output_logical_vector));
  } else if(is(input, "bnum")){
    return(bnum$new(output_logical_vector));
  } else {
    # Oooops!
    stop(input);
  }
}
