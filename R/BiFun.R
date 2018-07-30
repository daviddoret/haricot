library(R6);

#' @rdname BiFun_xxxx
BiFun <- R6Class(
  "BiFun",
  inherit = NandTree,
  public = list(
    initialize = function(
      truthtable) {
      truthtable_character <- convert_modbinum_any_to_modbinum_character(truthtable);
      input_dimension <- nchar(truthtable_character);
      super$initialize(
        input_dimension = input_dimension,
        output_dimension = 1);
      # Instanciates a specialized algorithm.
      bifun <- switch(
        truthtable_character,
        "00" = BiFun_00$new(),
        "10" = BiFun_10$new(),
        "01" = BiFun_01$new(),
        "11" = BiFun_11$new(),
        "0000" = BiFun_0000$new(),
        "1000" = BiFun_1000$new(),
        "0100" = BiFun_0100$new(),
        "1100" = BiFun_1100$new(),
        "0010" = BiFun_0000$new(),
        "1010" = BiFun_1000$new(),
        "0110" = BiFun_0100$new(),
        "1110" = BiFun_1100$new(),
        "0001" = BiFun_0000$new(),
        "1001" = BiFun_1000$new(),
        "0101" = BiFun_0100$new(),
        "1101" = BiFun_1100$new(),
        "0011" = BiFun_0000$new(),
        "1011" = BiFun_1000$new(),
        "0111" = BiFun_0100$new(),
        "1111" = BiFun_1100$new()
      );
      # Copy the internal logic of the specialized BiFun algorithm.
      self$logical_datatable <- bifun$get_logical_datatable();
    },
    do_randomize_outputs = function() {
      stop("Not supported");
    }
  )
)
