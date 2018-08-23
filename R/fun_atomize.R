#' atomize
#'
#' @description This transformation takes a composite or truth table in input, \cr
#' and returns a composite algorithm with equivalent logic,
#' that is only composed of atomic elements (NANDs and bit constants).
#'
#' @examples ...
#'
#' @param algo The original algorithm (R6 Class algo_base)
#' @param ... For future usage
#' @return A composite algorithm (R6 Class algo_composite)
#' @export
atomize <- function(algo, ...){

  if(is(algo, "algo_tt")){
    if(algo$get_dim_i() == 0){
      algo <- convert_tt_constant_to_composite(algo, ...);
    } else {
      algo <- split(algo);
    }
  };

  if(is(algo, "algo_composite")){
    components <- algo$get_components();
    if(length(components) > 0){
      for(component_index in 1 : length(components)){
        component <- components[[component_index]];
        if(component$get_is_atomic()){
          # This is atomic, let it like that.
        } else if(is(component, "algo_tt")){
          # atomize it recursively.
          atomized_component <- atomize(component);
          # And substitute it with the old one,
          # it the atomized component is different.
          if(atomized_component$get_algo_id() != component$get_algo_id()){
            algo$substitute(component, atomized_component, ...);
          }
        }
      }
    }
  }

  return(algo);

}
