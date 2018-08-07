library(R6);

#' @rdname BiFun_xxxx
AlgoTTxxxx <- R6Class(
  "AlgoComposite",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      truthtable) {
      truthtable_character <- do_convert_any_to_MoBiNum_character(truthtable);
      if(nchar(truthtable_character) != 4){
        stop("Truthtable size is not equal to 4, hence input dimension is not equal to 2");
      }
      super$initialize(
        input_dimension = 2,
        output_dimension = 1);
      # Instanciates a specialized algorithm.
      a1 <- switch(
        truthtable_character,
        "0000" = AlgoTT0000$new(),
        "1000" = AlgoTT1000$new(),
        "0100" = AlgoTT0100$new(),
        "1100" = AlgoTT1100$new(),
        "0010" = AlgoTT0010$new(),
        "1010" = AlgoTT1010$new(),
        "0110" = AlgoTT0110$new(),
        "1110" = AlgoTT1110$new(),
        "0001" = AlgoTT0001$new(),
        "1001" = AlgoTT1001$new(),
        "0101" = AlgoTT0101$new(),
        "1101" = AlgoTT1101$new(),
        "0011" = AlgoTT0011$new(),
        "1011" = AlgoTT1011$new(),
        "0111" = AlgoTT0111$new(),
        "1111" = AlgoTT1111$new()
      );
      # Steal the internal logic of the AlgoTTxxxx algorithm.
      self$do_copy_logic_from(a1);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
