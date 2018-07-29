library(R6);

#' @rdname BiFun_xxxx
BiFun_XOR <- R6Class(
  "BiFun_XOR",
  inherit = BiFun_0110,
  public = list(
    initialize = function() {
      super$initialize();
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
