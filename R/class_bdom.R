require(R6);
#' bdom (R6 class)
#'
#' @description \code{bdom} is an R6 implementation of the modular binary domain concept.
#'
#' @section References:
#' \itemize{
#' \item{Modular binary domain (\link{def_modular_binary_domain})}
#' }
#'
#' @examples bd1 <- bdom$new(1);
#' print(bd1);
#' bd2 <- bdom$new(2);
#' print(bd2);
#' bd3 <- bdom$new(3);
#' print(bd3);
#'
#' @param dimension The dimension of the modular binary domain. (integer)
#' @param ... For future usage.
#' @return An object instance of R6 class \code{bdom}.
#' @export
bdom <- R6Class(
  "Modular Binary Domain",
  public = list(
    # Private Members
    dimension = NULL,
    logical_matrix = NULL,
    initialize = function(dimension = 1, ...) {
      # Store private members
      self$dimension <- dimension;
      self$logical_matrix <- matrix(nrow = 2 ^ dimension, ncol = dimension);
      binum <- bnum$new(input = rep(FALSE, dimension));
      index <- 1;
      while(index <= 2 ^ dimension){
       self$logical_matrix[index,] <- binum$get_logical_vector();
       binum$do_increment();
       index <- index + 1;
      }
      rownames(self$logical_matrix) <- self$convert_to_character_vector();
    },
    convert_to_character_vector = function(...){
      return(convert_bdom_to_character_vector(self, ...));
    },
    get_dimension = function() {
      return(self$dimension);
    },
    get_length = function() {
      return(nrow(self$logical_matrix));
    },
    get_logical_matrix = function() {
      return(
        matrix(self$logical_matrix,
               ncol = self$get_dimension(),
               nrow = self$get_length()));
    },
    get_prettystring = function() {
      # Take a logical matrix reprensenting a truthtable and returns a "pretty" string representation.
      num <- as.numeric(self$logical_matrix);
      mat <- matrix(num, nrow = self$get_length());
      col <- apply(mat, 1, paste, collapse = "");
      pas <- paste(col, collapse = "\n");
      return(pas);
    },
    print = function(){
      cat(self$get_prettystring());
    }
  )
)
