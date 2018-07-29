library(R6);

#' @rdname BiFun_xxxx
BiFun_AND <- R6Class(
  "BiFun_AND",
  inherit = BiFun_0001,
  public = list(
    initialize = function(node_id) {
      super$initialize(node_id = node_id);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
