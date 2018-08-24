require(futile.logger);
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
#' exec_algo_composite(algo, input);
#'
#' # R6 method style:
#' algo$exec_algo_composite(input);
#'
#' @param algo A composite algorithm (R6 Class algo_composite)
#' @param input The input bits (logical vector | character vector of "0"s and "1"s | R6 Class BinaryNumber)
#' @return The corresponding output (same type than input)
#' @export
exec_algo_composite = function(algo, input = NULL, ...) {

  # Data validation.
  if(!is(algo, "algo_composite")){
    flog.error("exec_algo_composite: algo is not of algo_composite class");
    };
  if(is.null(input)){
    # Input is not mandatory, because the algorithm may be a constant with input dimension 0.
    # Strong assumption: in this situation, we default to the bnum type.
    flog.warn("exec_algo_tt: missing|NULL|NA input received --> default set to bnum(dim=0)");
    input <- bnum$new(dim = 0, ...);
  };

  # Applies the TruthTable algorithm and returns its output.
  # Returns a type that is consistent with the type of the input.
  input_logical_vector <- convert_any_to_logical_vector(input);

  if(length(input_logical_vector) != algo$get_dim_i()){
    flog.error("exec_algo_composite: algo dim_i <> input dim_i");
  }

  #flog.debug("exec_algo_composite(algo = %s (%s), input = %s)", algo$get_label(), algo$get_algo_id(), input);

  # Get a copy of the igraph to store execution values.
  g <- algo$get_dag();

  # Prepare the vertex values
  vertices_execution_value <- list();

  # Prepare the algorithm output.
  output_logical_vector <- rep(NA, algo$get_dim_o());

  push_execution <- function(
    vertex_name,
    pushed_value,
    pusher_position,
    ...){

    #flog.debug("push_execution(vertex_name = %s, pushed_value = %s, pusher_position % s, ...)",
    #           vertex_name,
    #           pushed_value,
    #           pusher_position);

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
      node <- algo$get_component(algo_id);
    }
    node_dim_i <- node$get_dim_i();
    # print(paste0("node_dim_i: ", node_dim_i));

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
        init_vector <- c(rep(NA, node_dim_i));
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

  # Push the algorithm execution from the atomic constants.
  # The rationale here is that atomic constants have an input dimension of 0,
  # hence their "execution" must be specifically pushed.
  # I decide to accomplish this first and to tackle input bits seconds,
  # but this is completely arbitrary.
  algo_list <- algo$get_components();
  if(length(algo_list) > 0){
    for(algo_index in 1:length(algo_list)){
      a <- algo_list[[algo_index]];
      # I explicitely test the class algo_0 or algo_1,
      # because a composite algorithm may also be a
      # logical constant with input dimension = 0.
      if(a$is_constant()) {
        # This is a constant.
        algo_id <- a$get_algo_id();
        # Prepare the name of the next vertex were to push the value.
        # For atomic constants, we know this is "o1" so we hard-code it.
        # But this is a bit ugly, isn't it?
        vertex_name <- baptize_igraph_vertex(algo_id, NOBIT_PREFIX);
        pushed_value <- a$exec(logical(0));
        push_execution(
          vertex_name = vertex_name,
          pushed_value = pushed_value,
          pusher_position = 1);
      };
    };
  };

  # Push the algorithm execution from the input bits.
  if(algo$get_dim_i() > 0){
    for(bit_position in 1:algo$get_dim_i()){
      bit <- paste0(INPUT_PREFIX, bit_position);
      algo_id <- algo$get_algo_id();
      vertex_name <- baptize_igraph_vertex(algo_id, bit);
      pushed_value <- input_logical_vector[bit_position];
      push_execution(
        vertex_name = vertex_name,
        pushed_value = pushed_value,
        pusher_position = bit_position);
    };
  };

  # Retrieve the result of the algo from the parent OutputBits.
  output_logical_vector <- rep(NA, algo$get_dim_o());
  for(position in 1:algo$get_dim_o()){
    bit <- paste0(OUTPUT_PREFIX, position);
    vertex_name <- paste0(algo$get_algo_id(), NAMESPACE_SEPARATOR, bit);
    bit_exec_value <- vertices_execution_value[[vertex_name]];
    if(is.na(bit_exec_value)){
      flog.error("missing output bit value after algo execution, the algo is flawed, please review its logical structure.");
    };
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
