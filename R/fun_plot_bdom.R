require(igraph);
require(RColorBrewer);
require(futile.logger);
require(ggplot2);
#' plot_bdom
#'
#' @description Generate a visualization of a binary domain,
#' following the general look and feel of the haricot package.
#'
#' @examples # R function style:
#' d <- bdom$new(3);
#' plot_bdom(d);
#'
#' # R6 class method style:
#' d$plot();
#'
#' @param d The binary domain to be plotted (R6 class bdom).
#' @param ... For future usage.
#' @return A plotted binary number.
#' @export
plot_bdom <- function(d, ...){

  if(!is(d, "bdom")){
    flog.error("plot_bdom: d is not of bdom type");
    stop();
  };

  # Prepare the input data for ggplot processing.
  prepare_data <- function(d){
    m <- d$get_logical_matrix();
    # Initializes the data.frame with the first column.
    plot_data <- data.frame(
      y = 0 : ((2 ^ d$get_dimension()) - 1),
      x = 1,
      z = as.numeric(m[,1]));
    # Loop on the remaining input columns.
    if(d$get_dimension() > 1){
      for(bit_pos in 2:d$get_dimension()){
        d3 <- data.frame(
          y = 0 : ((2 ^ d$get_dimension()) - 1),
          x = bit_pos,
          z = as.numeric(m[,bit_pos]));
        plot_data <- rbind(plot_data, d3);
      };
    };
    return(plot_data)
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

  prepare_plot(prepare_data(d, ...), ...);
}
