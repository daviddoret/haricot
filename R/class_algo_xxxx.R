library(R6);

#' @rdname BiFun_xxxx
algo_xxxx <- R6Class(
  "algo_composite",
  inherit = algo_composite,
  public = list(
    initialize = function(
      truthtable) {
      truthtable_character <- convert_any_to_bnum_character(truthtable);
      if(nchar(truthtable_character) != 4){
        stop("Truthtable size is not equal to 4, hence input dimension is not equal to 2");
      }
      super$initialize(
        input_dimension = 2,
        output_dimension = 1);
      # Instanciates a specialized algorithm.
      a1 <- switch(
        truthtable_character,
        "0000" = algo_0000$new(),
        "1000" = algo_1000$new(),
        "0100" = algo_0100$new(),
        "1100" = algo_1100$new(),
        "0010" = algo_0010$new(),
        "1010" = algo_1010$new(),
        "0110" = algo_0110$new(),
        "1110" = algo_1110$new(),
        "0001" = algo_0001$new(),
        "1001" = algo_1001$new(),
        "0101" = algo_0101$new(),
        "1101" = algo_1101$new(),
        "0011" = algo_0011$new(),
        "1011" = algo_1011$new(),
        "0111" = algo_0111$new(),
        "1111" = algo_1111$new()
      );
      # Steal the internal logic of the algo_xxxx algorithm.
      self$do_copy_logic_from(a1);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
