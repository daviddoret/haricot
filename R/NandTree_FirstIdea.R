#install.packages("data.table");
library(R6);
library(data.table);
library(igraph);
library(RColorBrewer)

#' NandTree_FirstIdea
#'
#' @export
NandTree_FirstIdea <- R6Class(
  "NandTree_FirstIdea",
  public = list(
    # Private Members
    input_dimension = NULL,
    output_dimension = NULL,
    last_nand_node_number = NULL,
    logical_datatable = NULL,
    initialize = function(input_dimension = 1, output_dimension = 1) {
      init_value <- FALSE;
      # Store private members
      self$input_dimension <- input_dimension;
      self$output_dimension <- output_dimension;
      self$last_nand_node_number <- 0;
      # Initializes the input nodes
      for(input_node_number in 1 : input_dimension){
        node_id <- paste0("i", input_node_number);
        self$set_input_node(node_id);
      }
      # Initializes the output nodes
      for(output_node_number in 1 : output_dimension){
        node_id <- paste0("o", output_node_number);
        self$set_output_node(node_id);
      }
    },
    do_apply_algorithm = function(input) {
      # Applies the TruthTable algorithm and returns its output.
      # Returns a type that is consistent with the type of the input.
      input_logical_vector <- convert_any_to_logical_vector(input);

      # Prepare a vector of answers.
      # This will help us compute every node only once.
      node_answers <- rep(NA, self$get_node_count());
      names(node_answers) <- self$logical_datatable[,"node_id"];

      # Assign the correct answers to the input nodes.
      for(input_node_number in 1:self$get_input_dimension()){
        node_id <- paste0("i", input_node_number);
        node_answers[node_id] <- input_logical_vector[input_node_number];
      }

      do_solve_node <- function(node_id){
        # Check if this is a proper node.
        if(node_id == "NA"){
          warning("Attempt to solve NA");
          answer <- "NA";
        } else if(1 == 2) {
          # TODO: Add here logic to check if the node does not exist.
          answer <- "NA";
        } else {
          # Check if we already have a final value for this node.
          answer <- node_answers[node_id];
          if(!is.na(answer)){
            # This is a "cache hit".
            message("Cache hit", node_id);
          } else {
            node <- self$get_node_by_node_id(node_id);
            if(node$type == "i"){
              # It should have been a "cache hit",
              # the input value should have answered this node.
              stop("Input Node without value.", node);
            } else if (node$type == "n"){
              # This is a NAND Node.
              sub_answer_1 <- do_solve_node(node$param1_id);
              sub_answer_2 <- do_solve_node(node$param2_id);
              if(sub_answer_1 == "NA" | sub_answer_2 == "NA"){
                warning("NAND Node based on NA", node);
                answer <- "NA";
              } else {
                answer <- do_nand(sub_answer_1, sub_answer_2);
              }
            } else if (node$type == "o"){
              # This is a NAND Node.
              sub_answer_1 <- do_solve_node(node$param1_id);
              if(sub_answer_1 == "NA"){
                warning("Output Node based on NA", node);
                answer <- "NA";
              } else {
                answer <- sub_answer_1;
              }
            }
          }
        }
        message("Node answer", node_id, answer);
        return(answer);
      }

      # Prepare the algorithm output.
      output_logical_vector <- rep(NA, self$get_output_dimension());

      # Loop on all output nodes.
      for(output_node_number in 1:self$get_output_dimension()){
        node_id <- paste0("o", output_node_number);
        output_node_output <- do_solve_node(node_id);
        if(output_node_output == "NA"){
          # To avoid issues when rbinding tables,
          # we improperly used the string "NA".
          output_node_output <- NA;
        }
        output_logical_vector[output_node_number] <- output_node_output;
      }

      if(is(input, "logical")){
        return(output_logical_vector);
      } else if(is(input, "character")){
        return(convert_logical_vector_to_character(output_logical_vector));
      } else if(is(input, "BinaryNumber_Modular")){
        return(BinaryNumber_Modular$new(output_logical_vector));
      } else {
        # Oooops!
        stop(input);
      }
    },
    do_plot = function() {
      plot_nandtree(self);
    },
    do_randomize_outputs = function() {
      # Randomizes the outputs of the TruthTable.

      # XXX
    },
    get_filter_by_node_id = function(node_id) {
      # Return a logical vector for filter purposes,
      # to retrieve the row where node_id = node_id.
      return(self$logical_datatable[, "node_id"] == node_id);
    },
    get_input_dimension = function() {
      return(self$input_dimension);
    },
    get_input_size = function() {
      # Returns the number of different input values.
      return(2 ^ self$get_input_dimension());
    },
    get_inverse = function() {

      # Find the dimensional input size of the original function.
      # This will become the output size of the inverse function.
      inverse_output_dimension <- self$get_input_dimension();

      # Find the dimensional output size of the original function.
      # This will become the input size of the inverse function.
      inverse_input_dimension <- self$get_output_dimension();

      # XXX.

      # Return the inverted truthtable.
      return(inverse_truthtable);

    },
    get_logical_datatable = function(){
      return(self$logical_datatable);
    },
    get_new_nand_node_id = function(){
      self$last_nand_node_number <- self$last_nand_node_number + 1;
      return(paste0("n", self$last_nand_node_number));
    },
    get_node_by_node_id = function(node_id) {
      filter <- self$get_filter_by_node_id(node_id);
      return(self$logical_datatable[filter,]);
    },
    get_node_count = function() {
      # Return the total number of nodes in the tree.
      return(length(self$logical_datatable[,]));
    },
    get_output_dimension = function() {
      return(self$output_dimension);
    },
    get_prettystring = function(){
      return(paste(self$logical_datatable[,"prettystring"], collapse = "\n"));
    },
    print = function(){
      cat(self$get_prettystring(), "\n");
    },
    set_any_node = function(type = "n", node_id = NULL, param1_id = "NA", param2_id = "NA"){
      if(type == "n" && is.null(node_id)){
        node_id <- self$get_new_nand_node_id();
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
        "i" = node_id,
        "n" = paste0(node_id,"=nand(",param1_id,",",param2_id,")"),
        "o" = paste0(node_id,"=",param1_id));
      row <- data.table(
        type = type,
        node_id = node_id,
        param1_id = param1_id,
        param2_id = param2_id,
        prettystring = prettystring,
        key = "node_id");
      if(is.null(self$logical_datatable)){
        # Initializes the data table if this is the first node.
        self$logical_datatable <- row;
      } else {
        filter <- self$get_filter_by_node_id(node_id);
        if(any(filter)){
          # This node exists already. We only need to update it.
          filtered_rows <- self$logical_datatable[!filter,];
          self$logical_datatable <- rbind(filtered_rows, row);
        } else {
          # This node does not exist, we need to insert it.
          self$logical_datatable <- rbind(self$logical_datatable, row);
        }
      }
      return(node_id);
    },
    set_input_node = function(node_id){
      node_id <- self$set_any_node(type = "i", node_id);
      return(node_id);
    },
    set_nand_node = function(node_id = NULL, param1_id = "NA", param2_id = "NA"){
      node_id <- self$set_any_node(type = "n", node_id = node_id, param1_id = param1_id, param2_id = param2_id);
      return(node_id);
    },
    set_output_node = function(node_id, param1_id = "NA"){
      node_id <- self$set_any_node(type = "o", node_id = node_id, param1_id = param1_id);
      return(node_id);
    }
  )
)
