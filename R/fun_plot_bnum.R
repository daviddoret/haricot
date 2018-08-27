require(igraph);
require(RColorBrewer);
require(futile.logger);
require(ggplot2);
#' plot_bnum
#'
#' @description Generate a visualization of a binary number,
#' following the general look and feel of the haricot package.
#'
#' @examples # R function style:
#' n <- bnum$new("1010111000");;
#' plot_bnum(n);
#'
#' # R6 class method style:
#' n$plot();
#'
#' @param n The binary number to be plotted.
#' @param ... For future usage.
#' @return A plotted binary number.
#' @export
plot_bnum <- function(n, ...){

  if(!is(n, "bnum")){
    flog.error("plot_bnum: n is not of bnum type");
    stop();
  };

  # Prepare the data for ggplot processing.
  prepare_data <- function(n, ...){
    # Initializes the data.frame with the first column.
    plot_data <- data.frame(
      y = 1,
      x = 1,
      z = as.numeric(n$get_logical_vector()[1]));
    # Loop on the remaining output columns.
    if(n$get_dimension() > 1){
      for(bit_pos in 2:n$get_dimension()){
        data_complement <- data.frame(
          y = 1,
          x = bit_pos,
          z = as.numeric(n$get_logical_vector()[bit_pos]));
        plot_data <- rbind(plot_data, data_complement);
      };
    };
    return(plot_data);
  };

  # Generate the plot.
  prepare_plot <- function(plot_data, ...){
    g <- ggplot(plot_data, aes(x = x, y = y)) +
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
      coord_fixed() +
      scale_x_continuous(
        breaks = c(1 : nrow(plot_data)),
        labels = paste0("b", c(1 : nrow(plot_data)))) +
      scale_fill_gradient(
        low = get_opt("BIT_0_COLOR"),
        high = get_opt("BIT_1_COLOR"));
    return(g);
  };

  prepare_plot(prepare_data(n, ...), ...);
}
