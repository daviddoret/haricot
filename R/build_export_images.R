# References:
# https://www.stat.berkeley.edu/~s133/saving.html
build_export_figures <- function(){

  a <- AlgoNAND$new();
  png("data/algo_nand_graph.png") #, 800, 800);
  a$do_plot();
  dev.off();
  png("man/figures/algo_nand_graph.png") #, 800, 800);
  a$do_plot();
  dev.off();

  a <- AlgoNOT$new();
  png("data/algo_not_graph.png") #, 800, 800);
  a$do_plot();
  dev.off();
  png("man/figures/algo_not_graph.png") #, 800, 800);
  a$do_plot();
  dev.off();


  }

#build_export_figures();
