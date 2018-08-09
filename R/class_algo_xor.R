library(R6);

#' @rdname algo_0110
#' @export
AlgoXOR <- R6Class(
  "AlgoXOR",
  inherit = algo_0110,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      super$initialize(
        algo_id,
        label,
        ...
      );
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
