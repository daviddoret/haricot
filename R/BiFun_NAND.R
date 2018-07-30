library(R6);

#' @rdname BiFun_xxxx
#' @export
BiFun_NAND <- R6Class(
  "BiFun_NAND",
  inherit = BiFun_1110,
  public = list(
    initialize = function() {
      super$initialize();
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
