library(R6);

#' @rdname BiFun_xxxx
BiFun_1010 <- R6Class(
  "BiFun_1010",
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

      # Inverse input bit 1.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i1");

      # Apply NAND to the two inputs.
      self$set_nand_subnode(
        subnode_id = "n2",
        param1_id = "i1",
        param2_id = "i2");

      # Apply NAND to the above two results.
      self$set_nand_subnode(
        subnode_id = "n3",
        param1_id = "n1",
        param2_id = "n2");

      # Inverse the result.
      self$set_nand_subnode(
        subnode_id = "n4",
        param1_id = "n3",
        param2_id = "n3");

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "n4");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
