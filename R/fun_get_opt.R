require(settings);
#' get_opt
#'
#' @description Set an option setting. \cr
#' By default it will be a globally defined setting. \cr
#' But package users may tweak their preferences by passing new settings in ...
#'
#' @param o The unique name of the option to be retrieve (character)
#' @param ... Overwrite package global option values with custom values.
#' @return The option setting value.
#' @export
GLOBAL_OPTIONS = options_manager(
  BIT_0_COLOR = "#cccce0",
  BIT_1_COLOR = "#9999e0",
  DEFAULT_DIM_I = 0, # Arbitrary default dimension. Required to simplify from_json().
  DEFAULT_DIM_O = 1, # Arbitrary default dimension. Required to simplify from_json().
  VERTIX_INPUTBIT_BGCOLOR = "#ccffe5",
  VERTIX_INPUTBIT_SHAPE = "circle",
  VERTIX_INPUTBIT_SIZE = 10,
  VERTIX_INPUTBIT_TYPE = "inputbit",
  VERTIX_OUTPUTBIT_BGCOLOR = "#cce5ff",
  VERTIX_OUTPUTBIT_SHAPE = "circle",
  VERTIX_OUTPUTBIT_SIZE = 10,
  VERTIX_OUTPUTBIT_TYPE = "outputbit"
);
get_opt <- function(o, ...){
  # protect against the use of reserved words.
  return(clone_and_merge(GLOBAL_OPTIONS, ...)(o));
};
