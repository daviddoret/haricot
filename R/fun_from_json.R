require(rjson);
#' from_json
#'
#' @description Convert a JSON export of a haricot class to an instance object of that class. \cr
#'
#' @examples ...
#'
#' @param json The exported haricot JSON (json / character).
#' @param instance (Conditional) The target object. If missing or null, a new object will be created (R6 class instance).
#' @param ...
#' @return The object instance (R6 class)
#' @export
from_json = function(json, instance = NULL, ...) {

  j <- rjson::fromJSON(json);
  o <- instance;

  # WARNING: I assume that class in position 1 is the top class in the hierarchy,
  # but I couldn't find this expressly documented.
  top_class <- j$classes[1];

  # WARNING: This is susceptible to code injection.
  if(is.null(o)){
    r <- paste0("function(){return(", top_class, "$new());};");
    f <- eval(parse(text = r));
    o <- f();
  };

  if(any(j$classes == "algo_base")){
    o$set_algo_id(j$algo_id);
    o$set_label(j$label);
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
  if(j$classes[1] == "algo_composite"){
    for(component_object in j$components){
      # Re-encode in JSON the component.
      component_json <- rjson::toJSON(component_object);
      # Recursively call from_json() on the component JSON.
      component_algo <- from_json(component_json);
      # Set the component on the newly inflated algo composite.
      # This will mechanically re-insufflate the vertices in the DAG.
      o$set_component(component_algo);
    };
    for(edge_object in j$edges){
      # Reset the original edges.
      o$set_edge(
        edge_object$source_node,
        edge_object_source_bit,
        edge_object$target_node,
        edge_object$target_bit);
    };
  };

  if(any(j$classes == "algo_tt")){
    # NOTE 1: Matrix are converted to vectors during JSONification.
    # But this is not an issue because the truth table dimensions are known
    # from the dim_i and dim_o attributes.
    # NOTE 2: In the JSON, we expressly convert the matrix to integer
    # in order to get 0s and 1s instead of TRUEs and FALSEs.
    # This makes the JSON more readable and compact.
    m <- matrix(
      as.logical(j$logical_matrix),
      ncol = j$dim_o,
      nrow = 2 ^ j$dim_i);
    o$set_logical_matrix(m);
  };

  return(o);

};
