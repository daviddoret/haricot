# Reference: http://r-pkgs.had.co.nz/man.html

require(sinew);

build_export_tabular <- function(){

  print("AlgoNOT");
  cat(tabular(AlgoNOT$new()$convert_to_algo_()$convert_to_character_dataframe(), heading=true));

  print("algo_0000");
  cat(tabular(algo_0000$new()$convert_to_algo_()$convert_to_character_dataframe(), heading=true));
  print("algo_0001");
  cat(tabular(algo_0001$new()$convert_to_algo_()$convert_to_character_dataframe(), heading=true));
  print("algo_0010");
  cat(tabular(algo_0010$new()$convert_to_algo_()$convert_to_character_dataframe(), heading=true));
  print("algo_0011");
  cat(tabular(algo_0011$new()$convert_to_algo_()$convert_to_character_dataframe(), heading=true));


}

#build_export_tabular();
