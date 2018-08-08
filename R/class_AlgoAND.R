library(R6);

#' @rdname algo_0001
#' @export
AlgoAND <- R6Class(
  "AlgoAND",
  inherit = algo_0001,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      super$initialize(
        node_id = node_id,
        label = label,
        ...
      );
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
