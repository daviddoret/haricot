library(R6);

#' @rdname BiFun_xxxx
BiFun_01 <- R6Class(
  "BiFun_01",
  inherit = NandTree,
  public = list(
    initialize = function() {
      super$initialize(
        input_dimension = 1,
        output_dimension = 1);

      self$set_output_subnode(
        subnode_id = "o1",
        param1_id = "i1");

    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
