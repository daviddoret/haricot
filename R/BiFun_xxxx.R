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
#' Finally, well-known logical gates have their dedicated alias classes:
#' \itemize{
#' \item \code{BiFun_AND},
#' \item \code{BiFun_NAND},
#' \item \code{BiFun_NOT},
#' \item \code{BiFun_OR},
#' \item \code{BiFun_XNOR},
#' \item \code{BiFun_XOR}.
#' }
#'
#' @usage # Specialized BiFun_xxxx R6 classes:
#' BiFun_00$new(node_id = "n1");
#' BiFun_10$new(node_id = "n2");
#' BiFun_0000$new(node_id = "n3");
#' BiFun_1000$new(node_id = "n4");
#' BiFun_0100$new(node_id = "n5");
#' # etc.
#'
#' Generic BiFun R6 class:
#' BiFun$new(node_id = "n6", truthtable = "00");
#' BiFun$new(node_id = "n7", truthtable = "10");
#' BiFun$new(node_id = "n8", truthtable = "0000");
#' BiFun$new(node_id = "n9", truthtable = "1000");
#' BiFun$new(node_id = "n10", truthtable = "0100");
#' # etc.
#'
#' Well-known logical gates R6 class:
#' BiFun_AND$new(node_id = "n11");
#' BiFun_XOR$new(node_id = "n12");
#'
#' @name BiFun_xxxx
NULL
