#' Notation
#'
#' The following notation conventions are used:
#'
#' @section Sets:
#' \itemize{
#' \item{ \eqn{\mathbb{B} }: The set of all modular binary numbers. }
#' \item{ \eqn{\mathbb{B}_{1} = {0,1} }: The set of all modular binary numbers of dimension 1. }
#' \item{ \eqn{\mathbb{B}_{2} = {00,10,01,11} }: The set of all modular binary numbers of dimension 2. }
#' \item{ \eqn{\mathbb{B}_{i} = {00...i,10...i,...} }: The set of all modular binary numbers of dimension i. }
#' \item{ \eqn{\mathbb{A}}: The set of all algorithms. }
#' \item{ \eqn{\mathbb{A}^{\mathbb{B}}}: The set of all algorithms that receive modular binary numbers as input and output modular binary numbers. }
#' \item{ \eqn{\mathbb{A}^{\mathbb{B}_{2},\mathbb{B}_{1}}}: The set of all algorithms that receive modular binary numbers of dimension 2 as input and output modular binary numbers of dimension 1. }
#' \item{ \eqn{\mathbb{A}^{\mathbb{B}_{i},\mathbb{B}_{j}}}: The set of all algorithms that receive modular binary numbers of dimension i as input and output modular binary numbers of dimension j. }
#' \item{ \eqn{\mathbb{T}^{\mathbb{A}}}: The set of all transformations that receive algorithms as inputs and output algorithms. }
#' }
#'
#' @section Modular Binary Number Constants:
#' Modular binary numbers are usually expressed as a sequence of "0" and "1" bits with the left-most bit being the least significant bit.
#' Examples:
#' \itemize{
#' \item{ \eqn{000} }
#' \item{ \eqn{110} }
#' \item{ \eqn{0000} }
#' \item{ \eqn{001100} }
#' }
#' If the context is ambiguous due to the presence of integers, modular binary numbers may be prefixed with a "b".
#' Examples:
#' \itemize{
#' \item{ \eqn{b000 } }
#' \item{ \eqn{b110 } }
#' \item{ \eqn{b0000 } }
#' \item{ \eqn{b001100 } }
#' }
#'
#' @section Variables:
#' \itemize{
#' \item{ \eqn{i,j,k \in \mathbb{N}^+}: Variables representing dimensions. }
#' \item{ \eqn{n \in \mathbb{B}}: Variables representing modular binary numbers. }
#' \item{ \eqn{a \in \mathbb{A}^{\mathbb{B}}}: Variables representing algorithms. }
#' \item{ \eqn{t \in \mathbb{T}^{\mathbb{A}x\mathbb{A}}}: Variables representing algorithms. }
#' }
#'
#' @section Bits of modular binary numbers:
#' To reference bits of modular binary numbers, we use the dot notation.
#' \itemize{
#' \item{ \eqn{n.2}: The second bit of n, starting from left. }
#' \item{ \eqn{n.i}: The ith bit of n, starting from left. }
#' }
#'
#' @section Input / output bits of algorithms:
#' To reference input / output bits of algorithms, we use the dot notation.
#' \itemize{
#' \item{ \eqn{a.i_{3}}: The 3rd input bit of a. }
#' \item{ \eqn{a.i_{i}}: The ith input bit of a. }
#' \item{ \eqn{a.o_{2}}: The 2nd output bit of a. }
#' \item{ \eqn{a.o_{i}}: The ith output bit of a. }
#' }
#'
#' @section Components of algorithm composites:
#' To reference components of algorithm composites, we use the dot notation:
#' \itemize{
#' \item{ \eqn{a_{composite}.a_{1}}: The first sub-algorithm of composite a. }
#' \item{ \eqn{a_{composite}.a_{i}}: The ith sub-algorithms of composite a. }
#' \item{ \eqn{a_{1}.o_{1} \rightarrow a_{2}.i_{1}}: The edge linking the output bit 1 of algo 1 to input bit 1 of algo 2. }
#' }
#'
#' @section Component predecessors and successors:
#' To reference predecessors and successors of algorithm components, we use:
#' \itemize{
#' \item{ \eqn{N^{-}(x)}: The immediate predecessors of x. }
#' \item{ \eqn{N^{+}(x)}: The immediate successors of x. }
#' }
#'
#' @section Dimension:
#' \itemize{
#' \item{ \eqn{dim(n)}: The dimension of n. }
#' \item{ \eqn{dim_{i}(a)}: The input dimension of a. }
#' \item{ \eqn{dim_{o}(a)}: The output dimension of b. }
#' }
#'
#' @name notation
NULL
