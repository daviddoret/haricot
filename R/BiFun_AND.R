library(R6);

#' @rdname BiFun_xxxx
BiFun_AND <- R6Class(
  "BiFun_AND",
  inherit = BiFun_0001,
  public = list(
    initialize = function(
      node_id = NULL,
      node_label = NULL,
      node_notes = NULL,
      node_style = NULL) {
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
