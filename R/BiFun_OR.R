library(R6);

#' @rdname BiFun_xxxx
BiFun_NOT <- R6Class(
  "BiFun_NOT",
  inherit = BiFun_10,
  public = list(
    initialize = function() {
      super$initialize();
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
