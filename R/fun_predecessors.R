require(checkmate);
#' predecessors
#'
#' In the context of a composite algorithm,
#' return the list of immediate predecessor algorithms
#' to a given component algorithm. \cr
#' \cr
#' If the component is a constant, the list of predecessors will be empty. \cr
#' \cr
#' Among the predecessors, we may find the composite if the component
#' is directly linked to one of the composite input bits. \cr
#' \cr
#' \strong{Notation} \cr
#' \deqn{N^{-}(x)} \cr
#'
#' @examples ...
#'
#' @param composite The composite within which we want to perform this operation (R6 class algo_composite).
#' @param component The component algorithm whose predecessors we want to list (R6 class algo_base | character vector of the component ID).
#' @return The list of predecessor algorithms (list of algo_base items).
#' @export
predecessors = function(composite, component, ...) {

  checkmate::assert_r6(composite, "algo_composite");
  checkmate::assert_r6(component, "algo_base");

  dag <- composite$get_dag();
  predecessors_list <- list();

  if(!(component$get_dim_i() == 0)){
    edge_filter <- (E(dag)$type == "inputbit_inputbit" |
      E(dag)$type == "outputbit_inputbit") &
      E(dag)$target_algo_id == component$get_algo_id();
    predecessors_ids <- unique(E(dag)[edge_filter]$source_algo_id);
    for(predecessor_id in predecessors_ids){
      predecessor <- NULL;
      if(predecessor_id == composite$get_algo_id()){
        predecessor <- composite;
      } else {
        predecessor <- composite$get_component(predecessor_id);
      };
      predecessors_list[[predecessor_id]] <- predecessor;
    };
  };
  return(predecessors_list);

};
