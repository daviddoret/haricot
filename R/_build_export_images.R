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

  export_figure("algo_10_graph", algo_10$new()$do_plot);

  export_figure("algo_0000_graph", algo_0000$new()$do_plot);
  export_figure("algo_0001_graph", algo_0001$new()$do_plot);
  export_figure("algo_0010_graph", algo_0010$new()$do_plot);
  export_figure("algo_0011_graph", algo_0011$new()$do_plot);

  export_figure("algo_1000_graph", algo_1000$new()$do_plot);
  export_figure("algo_1001_graph", algo_1001$new()$do_plot);
  export_figure("algo_1010_graph", algo_1010$new()$do_plot);
  export_figure("algo_1011_graph", algo_1011$new()$do_plot);

  export_figure("algo_0100_graph", algo_0100$new()$do_plot);
  export_figure("algo_0101_graph", algo_0101$new()$do_plot);
  export_figure("algo_0110_graph", algo_0110$new()$do_plot);
  export_figure("algo_0111_graph", algo_0111$new()$do_plot);

  export_figure("algo_1100_graph", algo_1100$new()$do_plot);
  export_figure("algo_1101_graph", algo_1101$new()$do_plot);
  export_figure("algo_1110_graph", algo_1110$new()$do_plot);
  export_figure("algo_1111_graph", algo_1111$new()$do_plot);

  export_figure("algo_and_graph", AlgoNOT$new()$do_plot);
  export_figure("algo_nand_graph", algo_nand$new()$do_plot);
  export_figure("algo_nor_graph", AlgoNOT$new()$do_plot);
  export_figure("algo_not_graph", AlgoNOT$new()$do_plot);
  export_figure("algo_or_graph", AlgoNOT$new()$do_plot);
  export_figure("algo_xnor_graph", AlgoNOT$new()$do_plot);


  }

#build_export_figures();
