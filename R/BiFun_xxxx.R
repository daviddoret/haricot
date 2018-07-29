#' Binary Function XXXX
#'
#' The most basic algorithm is NAND, that is the atomic building block of all higher-level algorithms.
#' \cr A second abstraction (or complexity) layer is composed of the binary functions (aka BiFun) of dimension 1x1 and 2x1.
#' \cr These are NAND trees that take 1 or 2 input bits and return 1 output bit.
#' \cr There are \eqn{2^2 + 2^4 = 4 + 16 = 20} distinct such functions.
#' \cr These are the building block functions from which any more complex algorithm may be built.
#' \cr There is one specialized R6 class for every binary function, in the form \code{BiFun_xxxx} where "xxxx" is the binary number representation of the algorithm's truth table.
#' \cr Amongst these, some are particularly useless, especially:
#' \cr \itemize{
#' \item \code{BiFun_00}: The constant 0.
#' \item \code{BiFun_11}: The constant 1.
#' \item \code{BiFun_0000}: The constant 0.
#' \item \code{BiFun_1111}: The constant 1.
#' }
#' There is a generic R6 class \code{BiFun} whose constructor takes a \code{truthtable} parameter of any supported type
#' that builds its internal NAND tree on the fly.
#'
#' @usage # Specialized BiFun_xxxx R6 classes:
#' BiFun_00$new();
#' BiFun_10$new();
#' BiFun_0000$new();
#' BiFun_1000$new();
#' BiFun_0100$new();
#' # etc.
#'
#' Generic BiFun R6 class:
#' BiFun$new(truthtable = "00");
#' BiFun$new(truthtable = "10");
#' BiFun$new(truthtable = "0000");
#' BiFun$new(truthtable = "1000");
#' BiFun$new(truthtable = "0100");
#' # etc.
#'
#' @name BiFun_xxxx
NULL
