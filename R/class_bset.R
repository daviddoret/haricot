require(R6);
require(ggplot2);
#' bset (R6 class)
#'
#' \code{bset} is the R6 class implementation of the \link[modular_binary_set]{modular binary set} concept.
#'
#' @section References:
#' \itemize{
#' \item{\link[modular_binary_set]{modular binary set}}
#' }
#'
#' @examples bd1 <- bset$new(1);
#' print(bd1);
#' bd2 <- bset$new(2);
#' print(bd2);
#' bd3 <- bset$new(3);
#' print(bd3);
#'
#' @param dimension The dimension of the modular binary set. (integer)
#' @param ... For future usage.
#' @return An object instance of R6 class \code{bset}.
#' @export
bset <- R6Class(
  "bset",
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
      return(convert_bset_to_character_vector(self, ...));
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
    plot = function(...){
      plot_bset(self, ...);
    },
    print = function(){
      cat(self$get_prettystring());
    }
  )
)
