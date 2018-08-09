library(R6);

#' @rdname algo_0001
#' @export
algo_and <- R6Class(
  "algo_and",
  inherit = algo_0001,
  public = list(
    initialize = function(
      algo_id = NULL,
      label = NULL,
      ...) {
      super$initialize(
        algo_id = algo_id,
        label = label,
        ...
      );
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
