library(R6);

#' @rdname BiFun_xxxx
BiFun_00 <- R6Class(
  "BiFun_00",
  inherit = NandTree,
  public = list(
    initialize = function() {
      super$initialize(
        input_dimension = 1,
        output_dimension = 1);

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

      # Invert output to get an unconditional FALSE.
      self$set_nand_subnode(
        subnode_id = "n3",
        param1_id = "n2",
        param2_id = "n2");

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "n3");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
