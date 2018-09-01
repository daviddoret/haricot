require(R6);
#install.packages("data.table");
require(data.table);
require(rlang);
require(igraph);
require(rlist);

#' algo_composite (R6 class)
#'
#' A algo_composite is an algorithm that is composed of sub-algorithms.
#' A algo_composite is itself of type algo_base, enabling complex and deep trees.
#' @export
algo_composite <- R6Class(
  "algo_composite",
  inherit = algo_base,
  private = list(
    components = NULL,
    dag = NULL
  ),
  public = list(
    # Constructor
    initialize = function(
      dim_i = get_open(DEFAULT_DIM_I),
      dim_o = get_open(DEFAULT_DIM_O),
      algo_id = NULL,
      label = NULL,
      ...) {
      # Call the super class constructor
      super$initialize(
        dim_i = dim_i,
        dim_o = dim_o,
        algo_id = algo_id,
        label = label,
        ...);
      private$components <- list();

      # WARNING: NEARLY REDUNDANT CODE WITH convert_algo_base_to_igraph
      private$dag <- make_empty_graph(directed = TRUE) %>%
        add_vertices(
          nv = self$get_dim_i(),
          bit = paste0(INPUT_PREFIX, 1:self$get_dim_i()),
          color = "#ccffe5",
          label = paste0(INPUT_PREFIX, 1:self$get_dim_i()),
          name = paste0(self$get_algo_id(), NAMESPACE_SEPARATOR, paste0(INPUT_PREFIX, 1:self$get_dim_i())),
          algo_id = self$get_algo_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "inputbit") %>%
        add_vertices(
          nv = self$get_dim_o(),
          bit = paste0(OUTPUT_PREFIX, 1:self$get_dim_o()),
          color = "#cce5ff",
          label = paste0(OUTPUT_PREFIX, 1:self$get_dim_o()),
          name = paste0(self$get_algo_id(),NAMESPACE_SEPARATOR,paste0(OUTPUT_PREFIX, 1:self$get_dim_o())),
          algo_id = self$get_algo_id(),
          push_execution_value = list(), # A vector of pushed execution values.
          shape = "circle",
          size = 10,
          type = "outputbit");
    },
    # Shortcut method to quickly add atomic NANDs.
    add_nand = function(
      source_1_node = NULL,
      source_1_bit,
      source_2_node = NULL,
      source_2_bit,
      target_node = NULL,
      target_bit = NULL,
      ...){
      if(is_missing(source_1_node) | is.null(source_1_node)){source_1_node <- self;}
      if(is_missing(source_2_node) | is.null(source_2_node)){source_2_node <- self;}
      if(is_missing(target_node) | is.null(target_node)){target_node <- self;}
      nand1 <- algo_nand$new(...);
      self$set_component(nand1, ...);
      self$set_dag_edge(source_1_node,source_1_bit,nand1,"i1", ...);
      self$set_dag_edge(source_2_node,source_2_bit,nand1,"i2", ...);
      if(!is.null(target_bit)){
        self$set_dag_edge(nand1,"o1",target_node,target_bit, ...);
      }
      return(nand1);
    },
    copy_logic_to = function(target){
      return(copy_logic(self, target));
    },
    copy_logic_from = function(source){
      return(copy_logic(source, self));
    },
    do_randomize_outputs = function() {
      stop("ooops");
    },
    exec = function(input = NULL, ...) {
      #log(obj = self, method = "exec", input = input, ...);
      return(exec_algo_composite(algo = self, input = input, ...));
    },
    get_component_count = function(){
      return(length(private$components));
    },
    get_component = function(inner_algo_id){
      return(private$components[[inner_algo_id]]);
    },
    get_component_predecessors = function(inner_algo_id){
      # Return a vector of node ids
      # corresponding to the direct predecessors of the target node.
      stop("ooops");
    },
    get_components = function(){
      return(private$components);
    },
    get_dag = function(){
      return(private$dag);
    },
    get_inverse = function() {
      stop("ooops");
    },
    get_prettystring = function(){
      # TODO: Enrich this with a nice representation of the algo inner logic.
      return(super$get_label());
    },
    increase_input_bits = function(n = NULL, ...){increase_input_bits(self, n, ...);},
    increase_output_bits = function(n = NULL, ...){increase_output_bits(self, n, ...);},
    plot = function(interactive = FALSE, ...) {plot_algo_composite(self, interactive, ...);},
    remove_component = function(component, ...){remove_component(self, component, ...);},
    set_component = function(node, ...){set_component(self, node, ...);},
    set_components = function(nodes, ...){private$components <- nodes;},
    set_dag = function(graph, ...){private$dag <- graph;},
    set_dag_edge = function(
      source_node = NULL,
      source_bit,
      target_node = NULL,
      target_bit,
      ...){
      set_graph_edge(
        self,
        source_node,
        source_bit,
        target_node,
        target_bit,
        ...);
    },
    set_dim_i = function(n, ...){
      previous_dim_i <- self$get_dim_i();
      if(previous_dim_i < n){
        # We must add new input bit vertices.
        increase_by <- n - previous_dim_i;
        flog.info("set_dim_i: increase input dimension by %s", increase_by);
        self$increase_input_bits(increase_by, ...);
      }
      if(previous_dim_i > n){
        # We must remove input bit vertices.
        flog.info("set_dim_i: decrease input dimension");
        stop("NOT IMPLEMENTED YET");
      }
    },
    system_set_dim_i = function(n, ...){
      private$dim_i <- n;
    },
    set_dim_o = function(n, ...){
      previous_dim_o <- self$get_dim_o();
      if(previous_dim_o < n){
        increase_by <- n - previous_dim_o;
        flog.info("set_dim_o: increase output dimension by %s", increase_by);
        self$increase_output_bits(increase_by, ...);
      };
      if(previous_dim_o > n){
        # We must remove output bit vertices.
        stop("NOT IMPLEMENTED YET");
      };
    },
    system_set_dim_o = function(n, ...){
      private$dim_o <- n;
    },
    substitute = function(original, substitute, ...){
      substitute(self, original, substitute, ...);
    }
  )
)
