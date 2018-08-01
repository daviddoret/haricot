require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);

#' CompositeAlgo
#'
#' A CompositeAlgo is an algorithm that is composed of sub-algorithms.
#' A CompositeAlgo is itself an CompositeAlgoInnerNode, enabling complex and deep trees.
#' @export
CompositeAlgo <- R6Class(
  "CompositeAlgo",
  inherit = CompositeAlgoInnerNode,
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

      private$inner_graph <- super$convert_to_igraph();

    },
    do_apply_algorithm = function(input) {
      stop("ooops");
    },
    do_plot = function() {
      plot(self$get_graph());
    },
    do_randomize_outputs = function() {
      stop("ooops");
    },
    get_graph = function(){
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
        source_node_id <- "_self"; #TODO: If we use a keyword like this,
                                   #ensure it can't be used as a normal id.
      }
      if(is_missing(target_node_id)){
        # If the inner_node is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        target_node_id <- "_self"; #TODO: If we use a keyword like this,
                                   #ensure it can't be used as a normal id.
      }
      #E(private$inner_graph)[[]]
    },
    set_inner_node = function(
      inner_node,
      inner_node_id = NULL,
      inner_node_label = ""){
      if(is_missing(inner_node_id) || is.null(inner_node_id)){
        inner_node_id <- get_node_guid();
      }
      if(is_missing(inner_node_label)){
        # TODO: Use a default_label attribute in CompositeAlgoInnerNode instead,
        # and enrich this with an automated numbering system,
        # e.g. "AND1", "AND2", etc. Or something like that...
        inner_node_label <- class(inner_node)[1];
      }
      inner_node_status <- "nok";
      private$inner_nodes[[inner_node_id]] <- inner_node;

      inner_node_graph <- inner_node$get_graph();
      V(inner_node_graph)$name <- paste0(
        inner_node_id, ".",V(inner_node_graph)$bit_id);

      V(inner_node_graph)$node_id <- inner_node_id;
      private$inner_graph <- graph.union(
        private$inner_graph,
        inner_node_graph,
        byname = TRUE);

      return(inner_node_id);
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
