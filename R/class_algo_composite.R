require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);
require(igraph);
require(rlist);

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
          bit = paste0(INPUT_PREFIX, 1:self$get_input_dimension()),
          color = "#ccffe5",
          label = paste0(INPUT_PREFIX, 1:self$get_input_dimension()),
          name = paste0(self$get_algo_id(), NAMESPACE_SEPARATOR, paste0(INPUT_PREFIX, 1:self$get_input_dimension())),
          algo_id = self$get_algo_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "inputbit") %>%
        add_vertices(
          nv = self$get_output_dimension(),
          bit = paste0(OUTPUT_PREFIX, 1:self$get_output_dimension()),
          color = "#cce5ff",
          label = paste0(OUTPUT_PREFIX, 1:self$get_output_dimension()),
          name = paste0(self$get_algo_id(),NAMESPACE_SEPARATOR,paste0(OUTPUT_PREFIX, 1:self$get_output_dimension())),
          algo_id = self$get_algo_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "outputbit")
    },
    copy_logic_to = function(target){
      return(copy_logic(self, target));
    },
    copy_logic_from = function(source){
      return(copy_logic(source, self));
    },
    exec = function(input, ...) {
      log(obj = self, method = "exec", input = input, ...);
      return(exec_algo_composite(algo = self, input = input, ...));
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
      # TODO: Enrich this with a nice representation of the algo inner logic.
      return(super$get_label());
    },
    # Shortcut method to quickly add atomic NANDs.
    add_nand = function(
      source_1_node = NULL,
      source_1_bit,
      source_2_node = NULL,
      source_2_bit,
      target_node = NULL,
      target_bit = NULL,
      ...){
      if(is_missing(source_1_node) | is.null(source_1_node)){source_1_node <- self;}
      if(is_missing(source_2_node) | is.null(source_2_node)){source_2_node <- self;}
      if(is_missing(target_node) | is.null(target_node)){target_node <- self;}
      nand1 <- algo_nand$new(...);
      self$set_inner_node(nand1, ...);
      self$set_inner_edge(source_1_node,source_1_bit,nand1,"i1", ...);
      self$set_inner_edge(source_2_node,source_2_bit,nand1,"i2", ...);
      if(!is.null(target_bit)){
        self$set_inner_edge(nand1,"o1",target_node,target_bit, ...);
      }
      return(nand1);
    },
    set_inner_edge = function(
      source_node = NULL,
      source_bit,
      target_node = NULL,
      target_bit,
      ...){
      set_graph_edge(
        self,
        source_node,
        source_bit,
        target_node,
        target_bit,
        ...);
    },
    remove_component = function(component, ...){
      remove_component(self, component, ...);
    },
    set_inner_node = function(node, ...){
      set_component(self, node, ...);
    },
    set_inner_graph = function(graph, ...){
      private$inner_graph <- graph;
    },
    set_inner_nodes = function(nodes, ...){
      private$inner_nodes <- nodes;
    },
    print = function(...){
      cat(self$get_prettystring(), "\n");
    }
  )
)
