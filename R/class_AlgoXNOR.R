library(R6);

#' @rdname AlgoTT1001
#' @export
AlgoXNOR <- R6Class(
  "AlgoXNOR",
  inherit = AlgoTT1001,
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
