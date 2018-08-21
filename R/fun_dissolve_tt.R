#' dissolve_tt
#'
#' @description This transformation takes a composite or truth table in input and returns a composite with identical logic.
#'
#' @examples ...
#'
#' @param source A truth table algorithm (R6 Class algo_tt)
#' @param ... For future usage
#' @return A composite algorithm (R6 Class algo_composite)
#' @export
dissolve_tt <- function(algo, ...){

  if(is(algo, "algo_tt")){
    if(algo$get_input_dimension() == 0){
      algo <- convert_tt_constant_to_composite(algo);
    } else {
      algo <- split(algo);
    }
  };

  if(is(algo, "algo_composite")){
    if(length(algo$get_inner_nodes()) > 0){
      for(algo_index in 1 : length(algo$get_inner_nodes())){
        sub_algo <- algo$get_inner_nodes()[algo_index];
        if(is(sub_algo$get_is_atomic())){
          # This is atomic, let it like that.
        } else
          # Dissolve it recursively.
          sub_dissolved <- dissolve_tt(sub_algo);
          # And substitute it with the old one.
          algo$substitute(sub_algo, sub_dissolved);
      }
    }
  }

  return(algo);

}
