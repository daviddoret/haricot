library(R6);

#' @rdname BiFun_xxxx
BiFun_11 <- R6Class(
  "BiFun_11",
  inherit = NandTree,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id, input_dimension = 1, output_dimension = 1);

      # Inverse the first input bit.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i1");

      # NAND the first input bit with its inverse.
      # The output is unconditionnaly TRUE.
      self$set_nand_subnode(
        subnode_id = "n2",
        param1_id = "i1",
        param2_id = "n1");

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "n2");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
