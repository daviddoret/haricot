library(R6);
#install.packages("data.table");
library(data.table);

#' CompositeTree
#'
#' A CompositeTree is an algorithm that is composed of AbstractNodes.
#' A CompositeTree is itself an AbstractNode, enabling complex and deep trees.
#' @export
CompositeTree <- R6Class(
  "CompositeTree",
  inherit = AbstractNode,
  private = list(
    edges = NULL,
    nodes = NULL
  ),
  public = list(
    # Constructor
    initialize = function(
      node_id = NULL,
      node_label = NULL,
      node_notes = NULL,
      node_style = NULL,
      input_dimension,
      output_dimension) {
      # Call the supercall constructor
      super$initialize(
        node_id = node_id,
        node_label = node_label,
        node_notes = node_notes,
        node_style = node_style,
        input_dimension = input_dimension,
        output_dimension = output_dimension);
      # Initializes the nodes list
      private$nodes <- list();
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
    get_edge_filter = function(
      source_subnode_id = NULL,
      source_subnode_bitid = NULL,
      target_subnode_id = NULL,
      target_subnode_bitid = NULL) {
      # Return a logical vector for filtering purposes on the edge datatable.
      # TODO: This code is probably awfully inefficient, clean this up.
      filter <- private$edges;
      if(!is.null(source_subnode_id)){
        filter <- filter[, "source_subnode_id"] == source_subnode_id;
      }
      if(!is.null(source_subnode_bitid)){
        filter <- filter[, "source_subnode_bitid"] == source_subnode_bitid;
      }
      if(!is.null(target_subnode_id)){
        filter <- filter[, "target_subnode_id"] == target_subnode_id;
      }
      if(!is.null(target_subnode_bitid)){
        filter <- filter[, "target_subnode_bitid"] == target_subnode_bitid;
      }
      return(filter);
    },
    get_inverse = function() {
      stop("ooops");
    },
    get_node = function(node_id){
      return(private$nodes[[node_id]]);
    },
    get_prettystring = function(){
      stop("ooops");
    },
    set_edge = function(
      source_subnode_id,
      source_subnode_bit_id,
      target_subnode_id,
      target_subnode_bit_id){
      prettystring <- paste0(
        source_subnode_id, ".", source_subnode_bit_id,
        " > ",
        target_subnode_id, ".", target_subnode_bit_id
        );
      new_edge <- data.table(
        source_subnode_id = source_subnode_id,
        source_subnode_bit_id = source_subnode_bit_id,
        target_subnode_id = target_subnode_id,
        target_subnode_bit_id = target_subnode_bit_id,
        prettystring = prettystring);
      if(is.null(private$edges)){
        # Initializes the data table if this is the first subnode.
        private$edges <- new_edge;
      } else {
        filter <- self$get_filter_by_subnode_id(
          target_subnode_id = target_subnode_id,
          target_subnode_bit_id = target_subnode_bit_id);
        if(any(filter)){
          # This target bit has already an incoming edge.
          # By definition, a single bit cannot bit targetted multiple times.
          # Hence we must assume the intention was to substitute the existing edge.
          filtered_rows <- private$edges[!filter,];
          private$edges <- rbind(filtered_rows, new_edge);
        } else {
          # This subnode does not exist, we need to insert it.
          private$edges <- rbind(private$edges, new_edge);
        }
      }
    },
    set_node = function(node){
      node_id <- node$get_node_id();
      private$nodes[[node_id]] <- node;
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
