#' convert_algo_tt_to_character_dataframe
#'
#' @description Takes an object of type algo_ and returns a character dataframe representation.
#' This format is both machine and human readable.
#' Useful to populate truth tables in ROxygen2 tabular format with sinew for package documentation.
#'
#' @examples # Function style:
#' a1 <- algo_tt$new(dim_i = 2, dim_o = 3);
#' convert_algo_tt_to_character_dataframe(a1);
#'
#' # R6 method style:
#' a1$convert_to_character_dataframe();
#'
#' @param algo_ A truth table alogirthm (R6 Class algo_)
#' @param ... For future usage
#' @return A nicely formatted ROxygen2 Tabular (character)
#' @export
convert_algo_tt_to_character_dataframe = function(
  algo_tt,
  ...) {
  m1 <- algo_tt$get_logical_matrix();
  input <- rownames(m1);
  output <- apply(
    X = m1,
    MARGIN = 1,
    FUN = convert_logical_vector_to_character);
  df1 <- data.frame(input = input, output = output);
  colnames(df1) <- c("input", "output");
  return(df1);
}
