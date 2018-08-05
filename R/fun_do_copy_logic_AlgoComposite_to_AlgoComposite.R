#' do_copy_logic_AlgoComposite_to_AlgoComposite, AlgoComposite$do_copy_logic_to, AlgoComposite$do_copy_logic_from
#'
#' @description Copies the internal logic of a composite algorithm on a different composite algorithm.
#'
#' @usage # R function style:
#' a1 <- AlgoXNOR$new();
#' a2 <- AlgoComposite$new(input_dimension = 2, output_dimension = 1);
#' do_copy_logic_AlgoComposite_to_AlgoComposite(a1, a2);
#' a2$do_plot();
#'
#' # R6 method style with copy from:
#' a3 <- AlgoComposite$new(input_dimension = 2, output_dimension = 1);
#' a3$do_copy_logic_from(a1);
#' a3$do_plot();
#'
#' # R6 method style with copy to:
#' a4 <- AlgoComposite$new(input_dimension = 2, output_dimension = 1);
#' a1$do_copy_logic_to(a4);
#' a4$do_plot();
#'
#' @param source A composite algorithm (R6 Class AlgoComposite)
#' @param target A composite algorithm (R6 Class AlgoComposite)
#' @param ... For future usage
#' @return N/A
#' @export
do_copy_logic_AlgoComposite_to_AlgoComposite <- function(source, target, ...){

  if(source$get_input_dimension() != target$get_input_dimension()){
    stop("source and target input dimensions are not identical");
  }
  if(source$get_output_dimension() != target$get_output_dimension()){
    stop("source and target output dimensions are not identical");
  }

  source_nodes <- source$get_inner_nodes();
  source_graph <- source$get_inner_graph();

  # Substitute node_id in inner graph
  stop("substitute edge$source_node_id");
  E(source_graph)[E(source_graph)$source_node_id == source$get_node_id()]$source_node_id <- target$get_node_id()
  stop("substitute edge$target_node_id");
  stop("substitute edge$name");

  # Push the new logic in the target composite algo
  target$set_inner_nodes(target_inner_nodes);
  target$set_inner_graph(target_inner_graph);

}
