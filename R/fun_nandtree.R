#install.packages("data.table");
library(R6);
library(data.table);
library(igraph);
library(RColorBrewer)

#' NAND tree.
#'
#' @export
NandTree <- R6Class(
  "NandTree",
  inherit = algo_base,
  public = list(
    last_nand_subnode_number = NULL,
    logical_datatable = NULL,
    initialize = function(
      input_dimension,
      output_dimension) {
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = output_dimension);
      init_value <- FALSE;
      self$last_nand_subnode_number <- 0;
      # Initializes the input subnodes
      for(input_subnode_number in 1 : input_dimension){
        subalgo_id <- paste0("i", input_subnode_number);
        self$set_input_subnode(subalgo_id);
      }
      # Initializes the output subnodes
      for(output_subnode_number in 1 : output_dimension){
        subalgo_id <- paste0("o", output_subnode_number);
        self$set_output_subnode(subalgo_id);
      }
    },
    convert_to_truthtable = function(){
      return(convert_nandtree_to_truthtable(self));
    },
    do_execute = function(input) {
      return(execute_algorithm_nandtree(self, input));
    },
    plot = function() {
      plot_nandtree(self);
    },
    do_randomize_outputs = function() {
      # Randomizes the outputs of the TruthTable.

      # TODO: Implement
      stop("Not implemented");
    },
    get_filter_by_subalgo_id = function(subalgo_id) {
      # Return a logical vector for filter purposes,
      # to retrieve the row where subalgo_id = subalgo_id.
      return(self$logical_datatable[, "subalgo_id"] == subalgo_id);
    },
    get_inverse = function() {

      # Find the dimensional input size of the original function.
      # This will become the output size of the inverse function.
      inverse_output_dimension <- self$get_input_dimension();

      # Find the dimensional output size of the original function.
      # This will become the input size of the inverse function.
      inverse_input_dimension <- self$get_output_dimension();

      # TODO: Implement.
      stop("Not implemented");

      # Return the inverted truthtable.
      return(inverse_truthtable);

    },
    get_logical_datatable = function(){
      return(self$logical_datatable);
    },
    get_new_nand_subalgo_id = function(){
      self$last_nand_subnode_number <- self$last_nand_subnode_number + 1;
      return(paste0("n", self$last_nand_subnode_number));
    },
    get_subnode_by_subalgo_id = function(subalgo_id) {
      filter <- self$get_filter_by_subalgo_id(subalgo_id);
      return(self$logical_datatable[filter,]);
    },
    get_subnode_count = function() {
      # Return the total number of subnodes in the tree.
      # return(length(self$logical_datatable[,]));
      return(nrow(self$logical_datatable));
    },
    get_prettystring = function(){
      return(paste(self$logical_datatable[,"prettystring"], collapse = "\n"));
    },
    set_any_subnode = function(type = "n", subalgo_id = NULL, param1_id = "NA", param2_id = "NA"){
      if(type == "n" && is.null(subalgo_id)){
        subalgo_id <- self$get_new_nand_subalgo_id();
      }
      param1_id <- switch(
        type,
        "i" = "NA",
        "n" = param1_id,
        "o" = param1_id);
      param2_id <- switch(
        type,
        "i" = "NA",
        "n" = param2_id,
        "o" = "NA");
      prettystring <- switch(
        type,
        "i" = subalgo_id,
        "n" = paste0(subalgo_id,"=nand(",param1_id,",",param2_id,")"),
        "o" = paste0(subalgo_id,"=",param1_id));
      row <- data.table(
        type = type,
        subalgo_id = subalgo_id,
        param1_id = param1_id,
        param2_id = param2_id,
        prettystring = prettystring,
        key = "subalgo_id");
      if(is.null(self$logical_datatable)){
        # Initializes the data table if this is the first subnode.
        self$logical_datatable <- row;
      } else {
        filter <- self$get_filter_by_subalgo_id(subalgo_id);
        if(any(filter)){
          # This subnode exists already. We only need to update it.
          filtered_rows <- self$logical_datatable[!filter,];
          self$logical_datatable <- rbind(filtered_rows, row);
        } else {
          # This subnode does not exist, we need to insert it.
          self$logical_datatable <- rbind(self$logical_datatable, row);
        }
      }
      return(subalgo_id);
    },
    set_input_subnode = function(subalgo_id){
      subalgo_id <- self$set_any_subnode(type = "i", subalgo_id);
      return(subalgo_id);
    },
    set_nand_subnode = function(subalgo_id = NULL, param1_id = "NA", param2_id = "NA"){
      subalgo_id <- self$set_any_subnode(type = "n", subalgo_id = subalgo_id, param1_id = param1_id, param2_id = param2_id);
      return(subalgo_id);
    },
    set_output_subnode = function(subalgo_id, param1_id = "NA"){
      subalgo_id <- self$set_any_subnode(type = "o", subalgo_id = subalgo_id, param1_id = param1_id);
      return(subalgo_id);
    }
  )
)
