#install.packages("uuid");
require("uuid");

#' Return a globally unique identifier for nodes.
#'
#' @description To enhance readability, it is desirable to provide a specific \code{algo_id} value for nodes.
#' But when none are provided, default globally unique identifiers will be used instead.
#'
#' @examples get_node_guid();
#'
#' @return A globally unique node identifier (character)
#' @export
get_node_guid = function() {
  return(as.character(uuid::UUIDgenerate(use.time = NA)));
}