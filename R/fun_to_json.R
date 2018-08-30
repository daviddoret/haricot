require(rjson);
#' to_json
#'
#' @description Convert an object from a haricot class to JSON.
#'
#' @examples ...
#'
#' @param o An object from one of the haricot package R6 classes (R6 Class)
#' @param ...
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

  json <- rjson::toJSON(j);

  return(json);
}

