library(R6);

#' @rdname algo_1001
#' @export
algo_xnor <- R6Class(
  "algo_xnor",
  inherit = algo_1001,
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
