require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);
require(igraph);

#' algo_composite (R6 class)
#'
#' A algo_composite is an algorithm that is composed of sub-algorithms.
#' A algo_composite is itself of type algo_base, enabling complex and deep trees.
#' @export
algo_composite <- R6Class(
  "algo_composite",
  inherit = algo_base,
  private = list(
    inner_nodes = NULL,
    inner_graph = NULL
  ),
  public = list(
    # Constructor
    initialize = function(
      input_dimension,
      output_dimension,
      algo_id = NULL,
      label = NULL,
      ...) {
      # Call the super class constructor
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension,
        algo_id = algo_id,
        label = label,
        ...);
      private$inner_nodes <- list();

      # WARNING: NEARLY REDUNDANT CODE WITH convert_algo_base_to_igraph
      private$inner_graph <- make_empty_graph(directed = TRUE) %>%
        add_vertices(
          nv = self$get_input_dimension(),
          bit = paste0("i", 1:self$get_input_dimension()),
          color = "#ccffe5",
          label = paste0("i", 1:self$get_input_dimension()),
          name = paste0(self$get_algo_id(), ".", paste0("i", 1:self$get_input_dimension())),
          algo_id = self$get_algo_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "inputbit") %>%
        add_vertices(
          nv = self$get_output_dimension(),
          bit = paste0("o", 1:self$get_output_dimension()),
          color = "#cce5ff",
          label = paste0("o", 1:self$get_output_dimension()),
          name = paste0(self$get_algo_id(),".",paste0("o", 1:self$get_output_dimension())),
          algo_id = self$get_algo_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "outputbit")
    },
    do_copy_logic_to = function(target){
      return(do_copy_logic_algo_composite_to_algo_composite(self, target));
    },
    do_copy_logic_from = function(source){
      return(do_copy_logic_algo_composite_to_algo_composite(source, self));
    },
    exec = function(input) {
      return(exec_algo_composite(algo = self, input = input));
    },
    plot = function() {
      plot_algo_composite(self);
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
    get_inner_node = function(inner_algo_id){
      return(private$inner_nodes[[inner_algo_id]]);
    },
    get_inner_node_predecessors = function(inner_algo_id){
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
    # Shortcut method to quickly add atomic NANDs.
    add_nand = function(
      source_1_node = NULL,
      source_1_bit,
      source_2_node = NULL,
      source_2_bit,
      target_node = NULL,
      target_bit = NULL){
      if(is.null(source_1_node)){source_1_node <- self;}
      if(is.null(source_2_node)){source_2_node <- self;}
      if(is.null(target_node)){target_node <- self;}
      nand1 <- algo_nand$new();
      self$set_inner_node(nand1);
      self$set_inner_edge(source_1_node,source_1_bit,nand1,"i1");
      self$set_inner_edge(source_2_node,source_2_bit,nand1,"i2");
      if(!is.null(target_bit)){
        self$set_inner_edge(nand1,"o1",target_node,target_bit);
      }
      return(nand1);
    },
    set_inner_edge = function(
      source_node,
      source_bit,
      target_node,
      target_bit){
      if(is_missing(source_node)){
        # If the inner_node is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        source_node <- self;
      }
      if(is_missing(target_node)){
        # If the inner_node is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        target_node <- self;
      }
      source_algo_id <- source_node$get_algo_id();
      target_algo_id <- target_node$get_algo_id();

      source_name <- paste0(source_algo_id, ".", source_bit);
      target_name <- paste0(target_algo_id, ".", target_bit);

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
        algo_id = self$get_algo_id(),
        source_algo_id = source_algo_id,
        source_bit = source_bit,
        target_algo_id = target_algo_id,
        target_bit = target_bit,
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
      algo_id <- node$get_algo_id();

      # Store the sub-algorithm node in the private list.
      private$inner_nodes[[algo_id]] <- node;

      # Retrieve the graph of the new node.
      node_graph <- node$convert_to_igraph();

      # Merge the current graph with the new one.
      private$inner_graph <- private$inner_graph %du% node_graph;
        #graph.union(
        #private$inner_graph,
        #node_graph,
        #byname = TRUE);

      # Of course, at this point the new sub-graph will be disconnected.
    },
    set_inner_graph = function(graph){
      private$inner_graph <- graph;
    },
    set_inner_nodes = function(nodes){
      private$inner_nodes <- nodes;
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)