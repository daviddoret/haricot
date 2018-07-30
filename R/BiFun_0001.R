library(R6);

#' @rdname BiFun_xxxx
BiFun_0001 <- R6Class(
  "BiFun_0001",
  inherit = NandTree,
  public = list(
    initialize = function() {
      super$initialize(
        input_dimension = 2,
        output_dimension = 1);

      # Apply NAND to the two input bits.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i2");

      # Inverse the result.
      self$set_nand_subnode(
        subnode_id = "n2",
        param1_id = "n1",
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
