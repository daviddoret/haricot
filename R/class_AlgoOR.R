library(R6);

#' @rdname algo_0111
#' @export
AlgoOR <- R6Class(
  "AlgoOR",
  inherit = algo_0111,
  public = list(
    initialize = function(
      node_id = NULL,
      label = NULL,
      ...) {
      super$initialize(
        node_id,
        label,
        ...
      );
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
