#' @title Switching Algorithms
#'
#' In the context of this package, switching algorithms is a procedure that consists in
#' taking two algorithms \eqn{a_{1}} and \eqn{a_{2}} of identical input and output dimensions,
#' and designing a new composite algorithm that incorporates the logic of both algorithms
#' with a complementary discriminant bit.
#'
#' REWRITE:
#' , is a composite algorithm c1 composed of 2 component algorithms: c2 and c3.
#' c1, c2 and c3 have identical output dimensions.
#' c2 and c3 have identical input dimensions.
#' c1 has input dimension of c2's or c3's input dimension + 1, the "extra bit" e.
#' The composite switch algorithm functions in the following manner:
#' 1). it receives the normal c2 or c3 input + the extra bit e.
#' 2). if e == 0, it runs c2 and returns its output.
#' 3). if e == 1, it runs c3 and returns its output.
#' In summary, a composite switch is a practical construction that combines two algorithms and switch their execution with an extra bit.
#'
#' @seealso \code{\link{commutate}}
#' @seealso \code{\link{unswitching_algorithms}}
#'
#' @name switching_algorithms
NULL;
