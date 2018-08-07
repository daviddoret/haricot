require(igraph);
require(RColorBrewer)
#' Plot the igraph of a AlgoNode
#'
#' @description Plot the igraph of a AlgoNode with adequate style, etc.
#'
#' @examples a1 <- AlgoNAND$new();
#' do_plot_AlgoNode(a1);
#'
#' @param ... For future usage.
#' @return A plotted network graph.
#' @export
do_plot_AlgoNode <- function(node, ...){

  g <- do_convert_AlgoNode_to_igraph(node = node, ...);

  # Make a palette of 3 colors
  vertice_fill_palette = brewer.pal(3, "Set1");
  vertice_fill_palette = c("#ffdddd", "#ddddff", "#ddffdd");
  names(vertice_fill_palette) <- c("inputbit_vertex", "algo_vertex", "outputbit_vertex");

  vertice_border_palette <- c("#660000", "#000066", "#006600");
  names(vertice_border_palette) <- c("inputbit_vertex", "algo_vertex", "outputbit_vertex");

  legend <- c("Input Bit", "Algorithm", "Output Bit");

  # Create a vector of color
  vertice_fill_color = vertice_fill_palette[V(g)$type];
  vertice_border_color = vertice_border_palette[V(g)$type];
  vertice_label_color <- vertice_border_color;

  # Make the plot
  plot(
    g,
    vertex.color=vertice_fill_color  ,
    edge.arrow.size=.1,
    vertex.size=20,
    vertex.frame.color=vertice_border_color,
    vertex.label.color=vertice_label_color,
    vertex.label.cex=0,
    vertex.label.dist=0,
    edge.curved=0.0
    #mark.groups=list(
    #  subnodes[input_filter],
    #  subnodes[nand_filter],
    #  subnodes[output_filter]) #, # draws polygon around subnodes
    #mark.border=vertice_label_color,
    #mark.col=,
    )

  # Add a legend
  #legend(
  #  "bottomleft",
  #  legend=legend  ,
  #      col = vertice_fill_palette ,
  #     bty = "n",
  #       pch=20 ,
  #       pt.cex = 3,
  #       cex = 1.5,
  #       text.col=vertice_fill_palette,
  #       horiz = FALSE,
  #       inset = c(0.1, 0.1));

}
