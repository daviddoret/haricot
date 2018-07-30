library(R6);

#' @rdname BiFun_xxxx
BiFun_1000 <- R6Class(
  "BiFun_1000",
  inherit = NandTree,
  public = list(
    initialize = function() {
      super$initialize(
        input_dimension = 2,
        output_dimension = 1);

      # Invert the first input bit.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i1");

      # Inverse the second input bit.
      self$set_nand_subnode(
        subnode_id = "n2",
        param1_id = "i2",
        param2_id = "i2");

      # Apply NAND to the two inverses.
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
