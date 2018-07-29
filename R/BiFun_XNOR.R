library(R6);

#' @rdname BiFun_xxxx
BiFun_XNOR <- R6Class(
  "BiFun_XNOR",
  inherit = BiFun_1001,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
