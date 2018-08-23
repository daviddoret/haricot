require(igraph);

#' substitute
#'
#' @description Substitute a component algorithm with another in a composite algorithm. \cr
#' The two component algorithms must be of equivalent dimensions.
#'
#' @examples # ...
#'
#' @param compi The composite algorithm inside which we want to perform the substitution (R6 Class algo_composite)
#' @param orig The original component algorithm that we want to extract from the composite (R6 Class algo_base)
#' @param subst The new component algorithm that we want to inject in the composite (R6 Class algo_base)
#' @param ... For future usage
#' @return N/A
#' @export
substitute <- function(compi, orig, subst, ...){

  if(orig$get_dim_i() != subst$get_dim_i()){
    stop("original and substitute input dimensions are not identical");
  }
  if(orig$get_dim_o() != subst$get_dim_o()){
    stop("original and substitute output dimensions are not identical");
  }

  if(!is(compi, "algo_composite")){
    stop("composite is not of class algo_composite");
  };

  if(!is(orig, "algo_base")){
    stop("original is not of class algo_base");
  };

  if(!is(subst, "algo_base")){
    stop("substitute is not of class algo_base");
  };

  # TODO: Check that the component algorithms
  # are effectively components of the composite.

  # Add the substite component to the composite.
  compi$set_component(subst, ...);

  # Retrieve the internals of the composite
  components <- compi$get_components();
  dag <- compi$get_dag();

  # Rewire the inbound edges with the substitute.
  inbound_edges <- E(dag)[
    E(dag)$source_algo_id != orig$get_algo_id() &
      E(dag)$target_algo_id == orig$get_algo_id()];
  for(edge_position in inbound_edges){
    # Retrieve the configuration from the edge.
    source_algo_id <- E(dag)[edge_position]$source_algo_id;
    source_algo <- NULL;
    if(source_algo_id == compi$get_algo_id()){
      source_algo <- compi;
    } else {
      source_algo <- compi$get_component(source_algo_id);
    ;}
    source_bit <- E(dag)[edge_position]$source_bit;
    target_bit <- E(dag)[edge_position]$target_bit;

    # Define the new configuration.
    target_algo <- subst;

    # Enforce the new configuration.
    compi$set_dag_edge(source_algo, source_bit, target_algo, target_bit, ...);
  }

  # Rewire the outbound edges with the substitute.
  outbound_edges <- E(dag)[
    E(dag)$source_algo_id == orig$get_algo_id() &
      E(dag)$target_algo_id != orig$get_algo_id()];
  for(edge_position in outbound_edges){
    # Retrieve the configuration from the edge.
    source_bit <- E(dag)[edge_position]$source_bit;
    target_algo_id <- E(dag)[edge_position]$target_algo_id;
    target_algo <- NULL;
    if(target_algo_id == compi$get_algo_id()){
      target_algo <- compi;
    } else {
      target_algo <- compi$get_component(target_algo_id);
      ;}
    target_bit <- E(dag)[edge_position]$target_bit;

    # Define the new configuration.
    source_algo <- subst;

    # Enforce the new configuration.
    compi$set_dag_edge(source_algo, source_bit, target_algo, target_bit, ...);
  }

  # Remove the original component.
  remove_component(compi, orig, ...);

  return(compi);
}
