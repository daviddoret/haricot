library(R6);

#' @rdname BiFun_xxxx
BiFun_1101 <- R6Class(
  "BiFun_1101",
  inherit = NandTree,
  public = list(
    initialize = function(
      node_id,
      node_label,
      node_notes,
      node_style) {
      super$initialize(
        node_id = node_id,
        node_label = node_label,
        node_notes = node_notes,
        node_style = node_style,
        input_dimension = 2,
        output_dimension = 1);

      # Apply NAND to the two input bits.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i2");

      # Apply NAND to the second input + the above NAND.
      self$set_nand_subnode(
        subnode_id = "n2",
        param1_id = "i2",
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
