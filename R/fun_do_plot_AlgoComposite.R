require(igraph);
require(RColorBrewer)
#' Plot the igraph of a AlgoComposite
#'
#' @description Plot the igraph of a AlgoComposite with adequate style, etc.
#'
#' @examples a1 <- AlgoXNOR$new();
#' do_plot_AlgoComposite(a1);
#'
#' @param algo A composite algorithm (AlgoComposite).
#' @param ... For future usage.
#' @return A plotted network graph.
#' @export
do_plot_AlgoComposite <- function(algo, ...){

  g <- do_convert_AlgoComposite_to_igraph(node = algo, ...);

  # Make a palette of 3 colors
  #vertice_fill_palette = brewer.pal(3, "Set1");
  #vertice_fill_palette = c("#ffdddd", "#ddddff", "#ddffdd");
  #names(vertice_fill_palette) <- c("inputbit", "algo", "outputbit");

  #vertice_border_palette <- c("#660000", "#000066", "#006600");
  #names(vertice_border_palette) <- c("inputbit", "algo", "outputbit");

  legend <- c("Input Bit", "Algorithm", "Output Bit");

  # Create a vector of color
  #vertice_fill_color = vertice_fill_palette[V(g)$type];
  #vertice_border_color = vertice_border_palette[V(g)$type];
  #vertice_label_color <- vertice_border_color;

  l <- layout_with_fr(g)

  mark_groups <- list();
  for(node_id in unique(V(g)$node_id)){
    numeric_vector <- which(V(g)$node_id == node_id);
    mark_groups[[node_id]] <- numeric_vector;
  }

  # Make the plot
  plot(
    g,
    #vertex.color=vertice_fill_color  ,
    #edge.arrow.size=.25,
    #vertex.frame.color=vertice_border_color,
    #vertex.label.color=vertice_label_color,
    vertex.label.cex=0,
    vertex.label.dist=0,
    edge.curved=0,
    layout = l,
    mark.groups=mark_groups,
    mark.border="#555555",
    mark.col="#ffffff"
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
