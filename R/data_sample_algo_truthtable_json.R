#' sample_algo_tt_json.RData
#'
#' A sample JSON exportation of a random truth table algorithm.
#'
#' @docType data
#'
#' @usage data(sample_algo_tt_json);
#'
#' @format JSON materialized as character.
#'
#' @keywords datasets
#'
#' @examples
#' # Load the sample data
#' data(sample_algo_tt_json, package = "haricot");
#' print(haricot::sample_algo_tt_json);
#' a1 <- algo_tt$new();
#' a1$from_json(haricot::sample_algo_tt_json);
#' a1$exec("1010");
#'
"sample_algo_tt_json";

# Function use to generate the data sample.
# Required at build time only, do not export.
generate_sample_algo_tt_json <- function(){
  a1 <- algo_tt$new(4,7);
  a1$do_randomize_outputs();
  sample_algo_tt_json <- a1$to_json();
  save(
    sample_algo_tt_json,
    list = "sample_algo_tt_json",
    file = "data/sample_algo_tt_json.RData");
};
