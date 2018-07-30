library(R6);

#' @rdname BiFun_xxxx
#' @export
BiFun_AND <- R6Class(
  "BiFun_AND",
  inherit = BiFun_0001,
  public = list(
    initialize = function() {
      super$initialize();
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
