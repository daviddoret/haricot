library(R6);

BinaryDomain <- R6Class(
  # BINUMOSET = Binary Number (Exhaustive) Ordered Set.
  # The binary n ordered set is defined as the ordered set of all binary values of dimension n.
  "BinaryDomain",
  public = list(
    # Private Members
    dimension = NULL,
    logical_matrix = NULL,
    initialize = function(dimension = 1) {
      # Store private members
      self$dimension <- dimension;
      self$logical_matrix <- matrix(nrow = 2 ^ dimension, ncol = dimension);
      binum <- BinaryNumber_Modular$new(input = rep(FALSE, dimension));
      index <- 1;
      while(index <= 2 ^ dimension){
       self$logical_matrix[index,] <- binum$get_logical_vector();
       binum$do_increment();
       index <- index + 1;
      }
    },
    get_dimension = function() {
      return(self$dimension);
    },
    get_length = function() {
      return(nrow(self$logical_matrix));
    },
    get_logical_matrix = function() {
      return(self$logical_matrix);
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

