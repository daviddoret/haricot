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

  export_figure("algo_tt10_graph", AlgoTT10$new()$do_plot);

  export_figure("algo_tt0000_graph", AlgoTT0000$new()$do_plot);
  export_figure("algo_tt0001_graph", AlgoTT0001$new()$do_plot);
  export_figure("algo_tt0010_graph", AlgoTT0010$new()$do_plot);
  export_figure("algo_tt0011_graph", AlgoTT0011$new()$do_plot);

  export_figure("algo_tt1000_graph", AlgoTT1000$new()$do_plot);
  export_figure("algo_tt1001_graph", AlgoTT1001$new()$do_plot);
  export_figure("algo_tt1010_graph", AlgoTT1010$new()$do_plot);
  export_figure("algo_tt1011_graph", AlgoTT1011$new()$do_plot);

  export_figure("algo_tt0100_graph", AlgoTT0100$new()$do_plot);
  export_figure("algo_tt0101_graph", AlgoTT0101$new()$do_plot);
  export_figure("algo_tt0110_graph", AlgoTT0110$new()$do_plot);
  export_figure("algo_tt0111_graph", AlgoTT0111$new()$do_plot);

  export_figure("algo_tt1100_graph", AlgoTT1100$new()$do_plot);
  export_figure("algo_tt1101_graph", AlgoTT1101$new()$do_plot);
  export_figure("algo_tt1110_graph", AlgoTT1110$new()$do_plot);
  export_figure("algo_tt1111_graph", AlgoTT1111$new()$do_plot);

  export_figure("algo_nand_graph", AlgoNAND$new()$do_plot);
  export_figure("algo_not_graph", AlgoNOT$new()$do_plot);

  }

#build_export_figures();
