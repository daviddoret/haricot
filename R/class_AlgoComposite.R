require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);
require(igraph);

#' AlgoComposite
#'
#' A AlgoComposite is an algorithm that is composed of sub-algorithms.
#' A AlgoComposite is itself of type AlgoNode, enabling complex and deep trees.
#' @export
AlgoComposite <- R6Class(
  "AlgoComposite",
  inherit = AlgoNode,
  private = list(
    inner_nodes = NULL,
    inner_graph = NULL
  ),
  public = list(
    # Constructor
    initialize = function(
      input_dimension,
      output_dimension,
      node_id = NULL) {
      # Call the supercall constructor
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        node_id = node_id);
      private$inner_nodes <- list();

      # WARNING: NEARLY REDUNDANT CODE WITH do_convert_AlgoNode_to_igraph
      private$inner_graph <- make_empty_graph(directed = TRUE) %>%
        add_vertices(
          nv = self$get_input_dimension(),
          bit_id = paste0("i", 1:self$get_input_dimension()),
          color = "#ccffe5",
          label = paste0("i", 1:self$get_input_dimension()),
          name = paste0(self$get_node_id(), ".", paste0("i", 1:self$get_input_dimension())),
          node_id = self$get_node_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "inputbit") %>%
        add_vertices(
          nv = self$get_output_dimension(),
          bit_id = paste0("o", 1:self$get_output_dimension()),
          color = "#cce5ff",
          label = paste0("o", 1:self$get_output_dimension()),
          name = paste0(self$get_node_id(),".",paste0("o", 1:self$get_output_dimension())),
          node_id = self$get_node_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "outputbit")
    },
    do_execute = function(input) {
      return(do_execute_AlgoComposite(algo = self, input = input));
    },
    do_plot = function() {
      do_plot_AlgoComposite(self);
    },
    do_randomize_outputs = function() {
      stop("ooops");
    },
    get_inner_graph = function(){
      return(private$inner_graph);
    },
    get_inner_node_count = function(){
      return(length(private$inner_nodes));
    },
    get_inner_node = function(inner_node_id){
      return(private$inner_nodes[[inner_node_id]]);
    },
    get_inner_node_predecessors = function(inner_node_id){
      # Return a vector of node ids
      # corresponding to the direct predecessors of the target node.
      stop("ooops");
    },
    get_inner_nodes = function(){
      return(private$inner_nodes);
    },
    get_inverse = function() {
      stop("ooops");
    },
    get_prettystring = function(){
      stop("ooops");
    },
    set_inner_edge = function(
      source_node_id,
      source_bit_id,
      target_node_id,
      target_bit_id){
      if(is_missing(source_node_id)){
        # If the inner_node is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        source_node_id <- self$get_node_id();
      }
      if(is_missing(target_node_id)){
        # If the inner_node is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        target_node_id <- self$get_node_id();
      }

      source_name <- paste0(source_node_id, ".", source_bit_id);
      target_name <- paste0(target_node_id, ".", target_bit_id);

      g <- self$get_inner_graph();

      # Determine the type of edge from node classes.
      source_vertex <- V(g)[V(g)$name == source_name];
      target_vertex <- V(g)[V(g)$name == target_name];
      type <- paste0(source_vertex$type, "_", target_vertex$type);
      # TODO: Check that we only manipulate manipulatable vertexes,
      # i.e. only InputBits and OutputBits.

      # InputBit and OutputBit nodes can only have a single inbound edge.
      # Delete the existing edges to guarantee graph consistency.
      g <- delete_edges(graph = g, edges = E(g)[to(target_name)]);

      color <- switch(
        type,
        "inputbit_inputbit" = "#00994c",
        "outputbit_outputbit" = "#004c99",
        "inputbit_outputbit" = "#00994c",
        "outputbit_inputbit" = "#004c99");

      new_edges <- c(source_name, target_name);
      #print(paste0("new_edges:", new_edges));
      #print(paste0("type:", type));
      #print(paste0("color:", color));

      g <- add_edges(
        graph = g,
        edges = new_edges,
        arrow.size = .1,
        arrow.width = 2,
        color = color,
        lty = "solid",
        type = type);
      # TODO: Add attributes for style, etc.

      private$inner_graph <- g;

    },
    set_inner_node = function(node){

      # TODO: If the node exists already, clean the igraph properly.

      # Retrieve the node unique ID
      node_id <- node$get_node_id();

      # Store the sub-algorithm node in the private list.
      private$inner_nodes[[node_id]] <- node;

      # Retrieve the graph of the new node.
      node_graph <- node$do_convert_to_igraph();

      # Merge the current graph with the new one.
      private$inner_graph <- private$inner_graph %du% node_graph;
        #graph.union(
        #private$inner_graph,
        #node_graph,
        #byname = TRUE);

      # Of course, at this point the new sub-graph will be disconnected.
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
