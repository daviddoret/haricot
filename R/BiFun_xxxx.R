#' Binary Function XXXX
#'
#' The most basic algorithm is NAND, that is the atomic building block of all higher-level algorithms.
#' \cr A second abstraction (or complexity) layer is composed of the binary functions (aka BiFun).
#' \cr These are NAND trees that take 2 input bits and return 1 output bit.
#' \cr There are \eqn{2^4 = 16} distinct such functions.
#' \cr These are the building block functions from which any more complex algorithm may be built.
#' \cr There is one R6 class for every binary function, in the form \code{BiFun_xxxx} where "xxxx" is the binary number representation of the algorithm's truth table.
#' \cr Amongst these, some are particularly useless, especially:
#' \cr \itemize{
#' \item \code{BiFun_0000}: The constant 0.
#' \item \code{BiFun_1111}: The constant 1.
#' }
#'
#' @usage BiFun_0000$new();
#' BiFun_1000$new();
#' BiFun_0100$new();
#' # etc.
#'
#' @name BiFun_xxxx
NULL
