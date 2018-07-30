require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);

#' CompositeDiGraph
#'
#' A CompositeDiGraph is an algorithm that is composed of AbstractNodes.
#' A CompositeDiGraph is itself an AbstractNode, enabling complex and deep trees.
#' @export
CompositeDiGraph <- R6Class(
  "CompositeDiGraph",
  inherit = AbstractNode,
  private = list(
    inner_edges = NULL,
    # data.table do not support the storage of objects.
    # Hence we must split storage in two:
    # - a list inner_nodes_obj for the node objects,
    # - a data.table inner_nodes_info for the complementary information.
    inner_nodes_obj = NULL,
    inner_nodes_info = NULL
  ),
  public = list(
    # Constructor
    initialize = function(
      input_dimension,
      output_dimension) {
      # Call the supercall constructor
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension);
      private$inner_nodes_obj <- list();
    },
    do_apply_algorithm = function(input) {
      stop("ooops");
    },
    do_plot = function() {
      stop("ooops");
    },
    do_randomize_outputs = function() {
      stop("ooops");
    },
    get_inner_edge_count = function(){
      return(nrow(private$inner_edges));
    },
    get_inner_edge_filter = function(
      source_node_id = NULL,
      source_bit_id = NULL,
      target_node_id = NULL,
      target_bit_id = NULL) {
      # Return a logical vector for filtering purposes on the edge datatable.
      # TODO: This code is probably awfully inefficient, clean this up.
      filter <- rep(TRUE, self$get_inner_edge_count());
      if(!is.null(source_node_id)){
        filter <- filter & private$inner_edges[, "source_node_id"] == source_node_id;
      }
      if(!is.null(source_bit_id)){
        filter <- filter & private$inner_edges[, "source_bit_id"] == source_bit_id;
      }
      if(!is.null(target_node_id)){
        filter <- filter & private$inner_edges[, "target_node_id"] == target_node_id;
      }
      if(!is.null(target_bit_id)){
        filter <- filter & private$inner_edges[, "target_bit_id"] == target_bit_id;
      }
      return(filter);
    },
    get_inner_node_info = function(inner_node_id){
      return(private$inner_nodes_info[
        self$get_inner_node_filter(inner_node_id = inner_node_id)]);
    },
    get_inner_node_object = function(inner_node_id){
      return(private$inner_nodes_obj[[inner_node_id]]);
    },
    get_inner_node_count = function(){
      return(length(private$inner_nodes_info));
    },
    get_inner_node_filter = function(
      inner_node_id = NULL,
      inner_node_depth = NULL) {
      # Return a logical vector for filtering purposes on the node datatable.
      # TODO: This code is probably awfully inefficient, clean this up.
      filter <- rep(TRUE, self$get_inner_node_count());
      if(!is.null(inner_node_id)){
        filter <- filter & private$inner_nodes[, "inner_node_id"] == inner_node_id;
      }
      if(!is.null(inner_node_depth)){
        filter <- filter & private$inner_nodes[, "inner_node_depth"] == inner_node_depth;
      }
      return(filter);
    },
    get_inner_nodes_info = function(){
      return(private$inner_nodes_info);
    },
    get_inner_nodes_obj = function(){
      return(private$inner_nodes_obj);
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
      prettystring <- paste0(
        source_node_id, ".", source_bit_id,
        " > ",
        target_node_id, ".", target_bit_id
        );
      new_inner_edge <- data.table(
        source_node_id = source_node_id,
        source_bit_id = source_bit_id,
        target_node_id = target_node_id,
        target_bit_id = target_bit_id,
        prettystring = prettystring);
      if(is.null(private$inner_edges)){
        # Initializes the data table if this is the first inner_node.
        private$inner_edges <- new_inner_edge;
      } else {
        filter <- self$get_inner_edge_filter(
          target_node_id = target_node_id,
          target_bit_id = target_bit_id);
        if(any(filter)){
          # This target bit has already an incoming edge.
          # By definition, a single bit cannot bit targetted multiple times.
          # Hence we must assume the intention was to substitute the existing edge.
          filtered_rows <- private$inner_edges[!filter,];
          private$inner_edges <- rbind(filtered_rows, new_inner_edge);
        } else {
          # This inner_node does not exist, we need to insert it.
          private$inner_edges <- rbind(private$inner_edges, new_inner_edge);
        }
      }
    },
    set_inner_node = function(
      inner_node,
      inner_node_id = NULL,
      inner_node_label = "",
      inner_node_notes = "",
      inner_node_style = ""){
      if(is_missing(inner_node_id) || is.null(inner_node_id)){
        inner_node_id <- get_node_guid();
      }
      if(is_missing(inner_node_label)){
        # TODO: Use a default_label attribute in AbstractNode instead,
        # and enrich this with an automated numbering system,
        # e.g. "AND1", "AND2", etc. Or something like that...
        inner_node_label <- class(inner_node)[1];
      }
      inner_node_depth <- -1;
      inner_node_status <- "nok";
      new_row <- data.table(
        inner_node_id = inner_node_id,
        inner_node_label = inner_node_label,
        inner_node_notes = inner_node_notes,
        inner_node_style = inner_node_style,
        inner_node_depth = inner_node_depth,
        inner_node_status = inner_node_status);
      private$inner_nodes_obj[[inner_node_id]] <- inner_node;
      if(is.null(private$inner_nodes_info)){
        # Initializes the data table if this is the first inner_node.
        private$inner_nodes_info <- new_row;
      } else {
        filter <- self$get_inner_node_filter(
          inner_node_id = inner_node_id);
        if(any(filter)){
          # If an attempt is made to set a node with the same node_id,
          # we must assume the intention was to substitute the existing node.
          filtered_rows <- private$inner_nodes_info[!filter,];
          private$inner_nodes_info <- rbind(filtered_rows, new_row);
        } else {
          # This inner_node does not exist, we need to insert it.
          private$inner_nodes_info <- rbind(private$inner_nodes_info, new_row);
        }
      }
      return(inner_node_id);
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
