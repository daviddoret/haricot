library(R6);

#' @rdname AlgoTT0110
#' @export
AlgoXOR <- R6Class(
  "AlgoXOR",
  inherit = AlgoTT0110,
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
