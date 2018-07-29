library(R6);

#' @rdname BiFun_xxxx
BiFun_1110 <- R6Class(
  "BiFun_1110",
  inherit = NandTree,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id, input_dimension = 2, output_dimension = 1);

      # Apply NAND to the two input bits.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i2");

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "n1");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
