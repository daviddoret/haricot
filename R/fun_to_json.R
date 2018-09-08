require(rjson);
#' to_json
#'
#' Convert an object from a haricot class to JSON.
#' Principle: the JSON produced should not contain more information than necessary to re-insufflate the object.
#'
#' @examples ...
#'
#' @param o An object from one of the haricot package R6 classes (R6 Class)
#' @param ... For future use.
#' @return JSON
#' @export
to_json = function(o, ...) {

  j <- list();
  j$classes <- class(o);

  if(is(o, "algo_base")){
    j$dim_i <- o$get_dim_i();
    j$dim_o <- o$get_dim_o();
    j$algo_id <- o$get_algo_id();
    j$label <- o$get_label();
    j$is_atomic <- o$get_is_atomic();
  };

  # There are classes inherited from algo_composite
  # that implement dynamically their internal logic
  # and make their internal logic read-only
  # (e.g. algo_1010).
  # In such situations, the internal logic should NOT be deJSONified.
  # For this reason, we only manage here top algo_composite classes.
  # WARNING: I could find a statement in the documentation stating
  # that top class must be in position 1, but it is. This could break
  # in future versions of R and/or R6.
  if(is(o, "algo_composite")){
    # Composite algorithms are resizable,
    # hence we need to store the input and output dimensions of the algo.
    j$dim_i <- o$get_dim_i();
    j$dim_i <- o$get_dim_i();
    # Components.
    j_comps <- list();
    for(i in 1 : o$get_component_count()){
      comp <- o$get_components()[[i]];
      # Recursively call to_json() on the component.
      j_comp <- comp$to_json();
      # We unJSONify the result
      # to avoid that the later JSONification processing
      # escapes the sub-JSON sequence as a string.
      o_comp <- rjson::fromJSON(j_comp);
      j_comps[[comp$get_algo_id()]] <- o_comp;
    }
    j$components <- j_comps;
    # NOTE: It is not necessary to JSONify the vertices,
    # because these can be re-inflated from the components list.
    # Edges.
    # We must filter the edges that are manually set,
    # and exclude the "system" edges that are mechanically
    # linking the input/output bits to their algo.
    filter <- E(o$get_dag())$source_bit != NOBIT_PREFIX &
      E(o$get_dag())$target_bit != NOBIT_PREFIX;
    o_edges <- E(o$get_dag())[filter];
    j_edges <- list();
    for(i in 1 : length(o_edges)){
      je <- list();
      je$source_algo_id <- o_edges[i]$source_algo_id;
      je$source_bit <- o_edges[i]$source_bit;
      je$target_algo_id <- o_edges[i]$target_algo_id;
      je$target_bit <- o_edges[i]$target_bit;
      j_edges[[i]] <- je;
    };
    j$edges <- j_edges;
  };

  if(is(o, "algo_tt")){
    # NOTE 1: Matrix are converted to vectors during JSONification.
    # But this is not an issue because the truth table dimensions are known
    # from the dim_i and dim_o attributes.
    # NOTE 2: In the JSON, we expressly convert the matrix to integer
    # in order to get 0s and 1s instead of TRUEs and FALSEs.
    # This makes the JSON more readable and compact.
    j$logical_matrix <- as.integer(o$get_logical_matrix());
  };

  json <- rjson::toJSON(j);

  return(json);
}

