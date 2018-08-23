require(igraph);
require(RColorBrewer)
#' Plot the igraph of a algo_composite
#'
#' @description Plot the igraph of a algo_composite with adequate style, etc.
#'
#' @examples a1 <- algo_xnor$new();
#' plot_algo_composite(a1);
#'
#' @param algo A composite algorithm (algo_composite).
#' @param ... For future usage.
#' @return A plotted network graph.
#' @export
plot_algo_composite <- function(algo, interactive = FALSE, ...){

  g <- convert_algo_composite_to_igraph(node = algo, ...);

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
  for(algo_id in unique(V(g)$algo_id)){
    numeric_vector <- which(V(g)$algo_id == algo_id);
    mark_groups[[algo_id]] <- numeric_vector;
  }

  if(!interactive){
  # Make the plot
  plot(
    g,
    #vertex.color=vertice_fill_color  ,
    edge.arrow.size=.1,
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
  } else {
    tkplot(g);
  };

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
