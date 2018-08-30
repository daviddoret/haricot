require(rjson);
#' from_json
#'
#' @description Convert a JSON export of a haricot class to an instance object of that class.
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

  return(o);

};
