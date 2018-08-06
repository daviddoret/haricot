library(R6);

#' @rdname BiFun_xxxx
AlgoTTxxxx <- R6Class(
  "AlgoComposite",
  inherit = AlgoComposite,
  public = list(
    initialize = function(
      truthtable) {
      truthtable_character <- do_convert_any_to_MoBiDum_character(truthtable);
      input_dimension <- nchar(truthtable_character);
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = 1);
      # Instanciates a specialized algorithm.
      a1 <- switch(
        truthtable_character,
        "0000" = AlgoTT0000$new(),
        "1000" = AlgoTT1000$new(),
        "0100" = AlgoTT0100$new(),
        "1100" = AlgoTT1100$new(),
        "0010" = AlgoTT0000$new(),
        "1010" = AlgoTT1000$new(),
        "0110" = AlgoTT0100$new(),
        "1110" = AlgoTT1100$new(),
        "0001" = AlgoTT0000$new(),
        "1001" = AlgoTT1000$new(),
        "0101" = AlgoTT0100$new(),
        "1101" = AlgoTT1100$new(),
        "0011" = AlgoTT0000$new(),
        "1011" = AlgoTT1000$new(),
        "0111" = AlgoTT0100$new(),
        "1111" = AlgoTT1100$new()
      );
      # Steal the internal logic of the AlgoTTxxxx algorithm.
      self$do_copy_logic_from(a1);
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
