require(futile.options);
#' Function: force_matrix
#'
#' @description The inner logic of truth table algorithms is composed of a logical matrix.
#' I observe that R implicitely converts 1 dimensional matrixes to vectors, etc.
#' This is not the behavior I want because I need to be able to work with the matrix independently of its size.
#' This function forces the creation of a matrix, even if it is 1-by-1, 1-by-x or x-by-1.
#'
#' @examples ...
#'
#' @param m A (perhaps already implicitely converted) logical matrix (logical matrix, vector or atomic logical value).
#' @param vector_direction VECTOR_DIRECTION_HORIZONTAL or VECTOR_DIRECTION_VERTICAL, depending on how we want vectors to be interpreted.
#' @return A strict matrix type of any size.
#' @name force_matrix
#' @export
force_matrix <- function(m = NULL, vector_direction = NULL, ...){
  if(is.null(vector_direction)){
    vector_direction <- VECTOR_DIRECTION_HORIZONTAL;
  };
  if(is.null(m)){
    m <- NULL;
  } else if(is.vector(m)){
    if(length(m) == 0){
      m <- matrix(logical(0));
    } else if(length(m) == 1){
      m <- matrix(data = m, nrow = 1, ncol = 1);
    } else {
      if(vector_direction == VECTOR_DIRECTION_HORIZONTAL){
        m <- matrix(data = m, nrow = 1, ncol = length(m));
      } else if(vector_direction == VECTOR_DIRECTION_VERTICAL){
        m <- matrix(data = m, nrow = length(m), ncol = 1);
      } else {
        flog.error("force_matrix: vector_direction not supported: %s", vector_direction);
      };
    };
  } else if(is.matrix(m)){
    m <- matrix(data = m, nrow = nrow(m), ncol = ncol(m));
  } else {
    flog.error("force_matrix: failed to force %s", m);
  };
  return(m);
};
