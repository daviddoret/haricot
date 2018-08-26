require(igraph);
require(RColorBrewer);
require(futile.logger);
require(ggplot2);
#' plot_algo_tt_inside
#'
#' @description Plot the internal logic of a truth table algorithm.
#'
#' @examples a1 <- algo_nand$new();
#' plot_algo_base(a1);
#'
#' @param algo The truth table algorithm to be plotted.
#' @param interactive (mentionned for consistency purpose, not yet supported).
#' @param ... For future usage.
#' @return A plotted truth table graph.
#' @export
plot_algo_tt_inside <- function(algo, interactive = FALSE, ...){

  if(!is(algo, "algo_tt")){
    flog.error("plot_algo_tt: algo is not of algo_tt type");
    stop();
  };

  # Prepare the input data for ggplot processing.
  prepare_input_data <- function(algo){
    i <- bdom$new(dim = algo$get_dim_i())$get_logical_matrix();
    # Initializes the data.frame with the first column.
    input_data <- data.frame(
      g = "Input",
      y = 0 : ((2 ^ algo$get_dim_i()) - 1),
      x = 1,
      z = as.numeric(i[,1]));
    # Loop on the remaining input columns.
    if(algo$get_dim_i() > 1){
      for(input_bit_pos in 2:algo$get_dim_i()){
        d3 <- data.frame(
          g = "Input",
          y = 0 : ((2 ^ algo$get_dim_i()) - 1),
          x = input_bit_pos,
          z = as.numeric(i[,input_bit_pos]));
        input_data <- rbind(input_data, d3);
      };
    };
    return(input_data)
  };

  # Prepare the output data for ggplot processing.
  prepare_output_data <- function(algo){
    # Initializes the data.frame with the first column.
    output_data <- data.frame(
      g = "Output",
      y = 0 : ((2 ^ algo$get_dim_i()) - 1),
      x = 1,
      z = as.numeric(algo$get_logical_matrix()[,1]));
    # Loop on the remaining output columns.
    if(algo$get_dim_o() > 1){
      for(output_bit_pos in 2:algo$get_dim_o()){
        d2 <- data.frame(
          g = "Output",
          y = 0 : ((2 ^ algo$get_dim_i()) - 1),
          x = output_bit_pos,
          z = as.numeric(algo$get_logical_matrix()[,output_bit_pos]));
        output_data <- rbind(output_data, d2);
      };
    };
    return(output_data);
  };

  # Put input and output data together in the same data.frame.
  consolidate_data <- function(algo){
    conso <- rbind(prepare_input_data(algo),
                   prepare_output_data(algo));
    return(conso);
  }

  # Plot the resulting truth table.
  sub_plot <- function(input_data){
    g <- ggplot(input_data, aes(x = x, y = y)) +
      geom_tile(
        aes(
          fill = z,
          height = 1,
          size = .5,
          width = 1),
        linetype = "solid",
        height = 1,
        width = 1,
        colour = "#444444",
        size = .1) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      scale_fill_gradient(low = "#cccce0", high = "#9999e0") +
      scale_y_reverse() +
      facet_wrap(~g, scales = "free_x");
    return(g);
  };

  sub_plot(consolidate_data(algo));
}
