# References:
# https://www.stat.berkeley.edu/~s133/saving.html

build_export_figures <- function(){

  export_figure <- function(filename, plot_function){
    png(paste0("data/", filename, ".png")); #, 800, 800);
    plot_function();
    dev.off();
    png(paste0("man/figures/", filename, ".png")); #, 800, 800);
    plot_function();
    dev.off();
  }

  export_figure("algo_10_graph", Algo10$new()$do_plot);
  export_figure("algo_1001_graph", Algo1001$new()$do_plot);
  export_figure("algo_nand_graph", AlgoNAND$new()$do_plot);
  export_figure("algo_not_graph", AlgoNOT$new()$do_plot);

  }

#build_export_figures();
