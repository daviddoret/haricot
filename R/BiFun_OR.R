library(R6);

#' @rdname BiFun_xxxx
#' @export
BiFun_OR <- R6Class(
  "BiFun_OR",
  inherit = BiFun_0111,
  public = list(
    initialize = function() {
      super$initialize();
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
