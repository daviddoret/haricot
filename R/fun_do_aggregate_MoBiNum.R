#' do_aggregate_MoBiNum
#'
#' @description ...
#'
#' @examples Function style:
#' n1 <- MoBiNum$new("0101");
#' n2 <- MoBiNum("11000");
#' n3 <- do_aggregate_MoBiNum(n1, n2);
#' print(n3);
#'
#' R6 method style:
#' n4 <- n1$aggregate_with(n2);
#' print(n4);
#'
#' @param ... Modular binary numbers (MoBiNum)
#' @return The result of the aggregation
#' @export
do_aggregate_MoBiNum <- function(...){

  dots <- list(...);
  if(length(dots) == 0) { stop("No MoBiNum provided") };



}
