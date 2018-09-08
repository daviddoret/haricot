require("uuid");
#' get_node_guid
#'
#' Return a pseudo-random globally unique identifier for nodes. \cr
#'
#' @examples get_node_guid();
#'
#' @return A globally unique node identifier (character)
#' @name get_node_guid
#' @export
get_node_guid = function() {
  return(as.character(uuid::UUIDgenerate(use.time = NA)));
};
