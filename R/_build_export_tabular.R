# Reference: http://r-pkgs.had.co.nz/man.html

require(sinew);

build_export_tabular <- function(){

  print("AlgoNOT");
  cat(tabular(AlgoNOT$new()$do_convert_to_AlgoTT()$do_convert_to_character_dataframe(), heading=true));

  print("AlgoTT0000");
  cat(tabular(AlgoTT0000$new()$do_convert_to_AlgoTT()$do_convert_to_character_dataframe(), heading=true));
  print("AlgoTT0001");
  cat(tabular(AlgoTT0001$new()$do_convert_to_AlgoTT()$do_convert_to_character_dataframe(), heading=true));
  print("AlgoTT0010");
  cat(tabular(AlgoTT0010$new()$do_convert_to_AlgoTT()$do_convert_to_character_dataframe(), heading=true));
  print("AlgoTT0011");
  cat(tabular(AlgoTT0011$new()$do_convert_to_AlgoTT()$do_convert_to_character_dataframe(), heading=true));


}

#build_export_tabular();
