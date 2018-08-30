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

  if(is(o, "algo_tt")){
    # NOTE 1: Matrix are converted to vectors during JSONification.
    # But this is not an issue because the truth table dimensions are known
    # from the dim_i and dim_o attributes.
    # NOTE 2: In the JSON, we expressly convert the matrix to integer
    # in order to get 0s and 1s instead of TRUEs and FALSEs.
    # This makes the JSON more readable and compact.
    j$logical_matrix <- as.integer(o$get_logical_matrix());
  }

  json <- rjson::toJSON(j);

  return(json);
}

