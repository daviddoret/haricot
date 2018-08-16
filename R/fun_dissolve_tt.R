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

  if(!is(algo, "algo_tt")){ stop("algo does not implement algo_tt");};

  # Split it in two.
  dissoled_algo <- split(algo, ...);

  # Recursively call the transform on the resulting composite.
  # This will recursively call the transform on its sub truth tables.
  dissoled_algo <- dissolve_tt(splitted, ...);
  return(recurse);

}
