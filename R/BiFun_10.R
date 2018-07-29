library(R6);

#' @rdname BiFun_xxxx
BiFun_10 <- R6Class(
  "BiFun_10",
  inherit = NandTree,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id, input_dimension = 1, output_dimension = 1);

      # Inverse the input bit.
      self$set_nand_subnode(
        subnode_id = "n1",
        param1_id = "i1",
        param2_id = "i1");

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "n1");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
