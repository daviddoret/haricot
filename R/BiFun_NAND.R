library(R6);

#' @rdname BiFun_xxxx
BiFun_NAND <- R6Class(
  "BiFun_NAND",
  inherit = BiFun_1110,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
