# References:
# https://www.stat.berkeley.edu/~s133/saving.html

build_export_figures <- function(){

  export_figure <- function(filename, plot_function){
    #png(paste0("data/", filename, ".png")); #, 800, 800);
    #plot_function();
    #dev.off();
    png(paste0("man/figures/", filename, ".png")); #, 800, 800);
    plot_function();
    dev.off();
  }

  export_figure("algo_0_graph", algo_0$new()$plot);
  export_figure("algo_1_graph", algo_1$new()$plot);

  export_figure("algo_00_graph", algo_00$new()$plot);
  export_figure("algo_10_graph", algo_10$new()$plot);
  export_figure("algo_01_graph", algo_01$new()$plot);
  export_figure("algo_11_graph", algo_11$new()$plot);

  export_figure("algo_0000_graph", algo_0000$new()$plot);
  export_figure("algo_0001_graph", algo_0001$new()$plot);
  export_figure("algo_0010_graph", algo_0010$new()$plot);
  export_figure("algo_0011_graph", algo_0011$new()$plot);

  export_figure("algo_1000_graph", algo_1000$new()$plot);
  export_figure("algo_1001_graph", algo_1001$new()$plot);
  export_figure("algo_1010_graph", algo_1010$new()$plot);
  export_figure("algo_1011_graph", algo_1011$new()$plot);

  export_figure("algo_0100_graph", algo_0100$new()$plot);
  export_figure("algo_0101_graph", algo_0101$new()$plot);
  export_figure("algo_0110_graph", algo_0110$new()$plot);
  export_figure("algo_0111_graph", algo_0111$new()$plot);

  export_figure("algo_1100_graph", algo_1100$new()$plot);
  export_figure("algo_1101_graph", algo_1101$new()$plot);
  export_figure("algo_1110_graph", algo_1110$new()$plot);
  export_figure("algo_1111_graph", algo_1111$new()$plot);

  export_figure("algo_and_graph", algo_not$new()$plot);
  export_figure("algo_nand_graph", algo_nand$new()$plot);
  export_figure("algo_nor_graph", algo_not$new()$plot);
  export_figure("algo_not_graph", algo_not$new()$plot);
  export_figure("algo_or_graph", algo_not$new()$plot);
  export_figure("algo_xnor_graph", algo_not$new()$plot);

  export_figure("bdom_1", bdom$new(1)$plot);
  export_figure("bdom_2", bdom$new(2)$plot);
  export_figure("bdom_3", bdom$new(3)$plot);
  export_figure("bdom_4", bdom$new(4)$plot);
  export_figure("bdom_5", bdom$new(5)$plot);
  export_figure("bdom_6", bdom$new(6)$plot);


  truthtable <- algo_tt$new(8,8)$do_randomize_outputs();
  dag <- atomize(truthtable);
  export_figure("algo_8_8", dag$plot);

  }

#build_export_figures();
