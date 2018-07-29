require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);

#' CompositeTree
#'
#' A CompositeTree is an algorithm that is composed of AbstractNodes.
#' A CompositeTree is itself an AbstractNode, enabling complex and deep trees.
#' @export
CompositeTree <- R6Class(
  "CompositeTree",
  inherit = AbstractNode,
  private = list(
    subedges = NULL,
    inner_nodes = NULL
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
      private$inner_nodes <- list();
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
    get_subedge_filter = function(
      source_node_id = NULL,
      source_bit_id = NULL,
      target_node_id = NULL,
      target_bit_id = NULL) {
      # Return a logical vector for filtering purposes on the edge datatable.
      # TODO: This code is probably awfully inefficient, clean this up.
      filter <- private$subedges;
      if(!is.null(source_node_id)){
        filter <- filter[, "source_node_id"] == source_node_id;
      }
      if(!is.null(source_bit_id)){
        filter <- filter[, "source_bit_id"] == source_bit_id;
      }
      if(!is.null(target_node_id)){
        filter <- filter[, "target_node_id"] == target_node_id;
      }
      if(!is.null(target_bit_id)){
        filter <- filter[, "target_bit_id"] == target_bit_id;
      }
      return(filter);
    },
    get_inverse = function() {
      stop("ooops");
    },
    get_subnode = function(subnode_id){
      return(private$subnodes[[subnode_id]]);
    },
    get_prettystring = function(){
      stop("ooops");
    },
    set_subedge = function(
      source_node_id,
      source_bit_id,
      target_node_id,
      target_bit_id){
      if(is_missing(source_node_id)){
        # If the subnode is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        source_node_id <- self$get_node_id();
      }
      if(is_missing(target_node_id)){
        # If the subnode is not specified,
        # we assume the intention is to work directly
        # on the input and output bits of the current node.
        target_node_id <- self$get_node_id();
      }
      prettystring <- paste0(
        source_node_id, ".", source_bit_id,
        " > ",
        target_node_id, ".", target_bit_id
        );
      new_subedge <- data.table(
        source_node_id = source_node_id,
        source_bit_id = source_bit_id,
        target_node_id = target_node_id,
        target_bit_id = target_bit_id,
        prettystring = prettystring);
      if(is.null(private$subedges)){
        # Initializes the data table if this is the first inner_node.
        private$subedges <- new_subedge;
      } else {
        filter <- self$get_subedge_filter(
          target_node_id = target_node_id,
          target_bit_id = target_bit_id);
        if(any(filter)){
          # This target bit has already an incoming edge.
          # By definition, a single bit cannot bit targetted multiple times.
          # Hence we must assume the intention was to substitute the existing edge.
          filtered_rows <- private$subedges[!filter,];
          private$subedges <- rbind(filtered_rows, new_subedge);
        } else {
          # This inner_node does not exist, we need to insert it.
          private$subedges <- rbind(private$subedges, new_subedge);
        }
      }
    },
    set_inner_node = function(inner_node){
      inner_node_id <- inner_node$get_node_id();
      private$inner_nodes[[inner_node_id]] <- inner_node;
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    }
  )
)
