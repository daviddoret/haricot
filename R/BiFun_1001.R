library(R6);

#' @rdname BiFun_xxxx
BiFun_1001 <- R6Class(
  "BiFun_1001",
  inherit = NandTree,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id, input_dimension = 2, output_dimension = 1);

      # Apply NAND to the two inputs
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i2");

      # Invert the first input bit.
      self$set_nand_subnode(
        subnode_id = "n2",
        param1_id = "i1",
        param2_id = "i1");

      # Inverse the second input bit.
      self$set_nand_subnode(
        subnode_id = "n3",
        param1_id = "i2",
        param2_id = "i2");

      # Apply NAND to the two inverses.
      self$set_nand_subnode(
        subnode_id = "n4",
        param1_id = "n2",
        param2_id = "n3");

      # Apply NAND to the two NANDs
      self$set_nand_subnode(
        subnode_id = "n5",
        param1_id = "n1",
        param2_id = "n4");

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "n5");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
