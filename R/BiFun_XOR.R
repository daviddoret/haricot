library(R6);

#' @rdname BiFun_xxxx
BiFun_XOR <- R6Class(
  "BiFun_XOR",
  inherit = BiFun_0110,
  public = list(
    initialize = function(
      node_id,
      node_label,
      node_notes,
      node_style) {
      super$initialize(
        node_id = node_id,
        node_label = node_label,
        node_notes = node_notes,
        node_style = node_style);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
