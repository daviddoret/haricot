library(R6);

#' @rdname algo_0111
#' @export
algo_or <- R6Class(
  "algo_or",
  inherit = algo_0111,
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
