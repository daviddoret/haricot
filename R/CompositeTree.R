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
      node_id,
      input_dimension,
      output_dimension) {
      # Call the supercall constructor
      super$initialize(
        node_id = node_id,
        input_dimension = input_dimension,
        output_dimension = output_dimension);
      # Initializes the nodes list
      self$nodes <- list();
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
      filter <- self$logical_datatable;
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
    get_node <- function(node_id){
      return(self$nodes[[node_id]]);
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
      if(is.null(self$edges)){
        # Initializes the data table if this is the first subnode.
        self$edges <- new_edge;
      } else {
        filter <- self$get_filter_by_subnode_id(
          target_subnode_id = target_subnode_id,
          target_subnode_bit_id = target_subnode_bit_id);
        if(any(filter)){
          # This target bit has already an incoming edge.
          # By definition, a single bit cannot bit targetted multiple times.
          # Hence we must assume the intention was to substitute the existing edge.
          filtered_rows <- self$edges[!filter,];
          self$edges <- rbind(filtered_rows, new_edge);
        } else {
          # This subnode does not exist, we need to insert it.
          self$edges <- rbind(self$edges, new_edge);
        }
      }
    },
    set_node <- function(node){
      node_id <- node$get_node_id();
      self$nodes[[node_id]] <- node;
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
