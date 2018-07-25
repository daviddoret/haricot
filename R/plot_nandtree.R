

#install.packages("igraph");
#install.packages("RColorBrewer");
library(igraph);
library(RColorBrewer)

plot_nandtree <- function(nandtree){

  input_filter <- nandtree$get_logical_datatable()$type == "i";
  nand_filter <- nandtree$get_logical_datatable()$type == "n";
  output_filter <- nandtree$get_logical_datatable()$type == "o";

  nodes <- c(nandtree$get_logical_datatable()$node_id);
  vertices <- data.frame(
    name = nandtree$get_logical_datatable()$node_id,
    type = nandtree$get_logical_datatable()$type);
  param1 <- nandtree$get_logical_datatable()$param1_id;
  param2 <- nandtree$get_logical_datatable()$param2_id;

  nand1_edges <- data.frame(source = param1[nand_filter], target = nodes[nand_filter]);
  nand2_edges <- data.frame(source = param2[nand_filter], target = nodes[nand_filter]);
  output_edges <- data.frame(source = param1[output_filter], target = nodes[output_filter]);
  edges_all <- rbind(nand1_edges, nand2_edges, output_edges);

  #edges_all
  #vertices

  g1 <- graph_from_data_frame(d = edges_all, vertices = vertices);

  #par(bg="grey32", mar=c(0,0,0,0));

  # Make a palette of 3 colors
  vertice_fill_palette = brewer.pal(3, "Set1");
  vertice_fill_palette = c("#ffdddd", "#ddddff", "#ddffdd");
  names(vertice_fill_palette) <- c("i", "n", "o");

  vertice_border_palette <- c("#660000", "#000066", "#006600");
  names(vertice_border_palette) <- c("i", "n", "o");

  legend <- c("Input", "NAND", "Output");

  # Create a vector of color
  vertice_fill_color = vertice_fill_palette[vertices$type];
  vertice_border_color = vertice_border_palette[vertices$type];
  vertice_label_color <- vertice_border_color;
  # Make the plot
  plot(
    g1,
    vertex.color=vertice_fill_color  ,
    edge.arrow.size=.25,
    vertex.size=20,
    vertex.frame.color=vertice_border_color,
    vertex.label.color=vertice_label_color,
    vertex.label.cex=0,
    vertex.label.dist=0,
    edge.curved=0.0,
    mark.groups=list(
      nodes[input_filter],
      nodes[nand_filter],
      nodes[output_filter]) #, # draws polygon around nodes
    #mark.border=vertice_label_color,
    #mark.col=,
    )

  # Add a legend
  legend(
    "bottomleft",
    legend=legend  ,
         col = vertice_fill_palette ,
         bty = "n",
         pch=20 ,
         pt.cex = 3,
         cex = 1.5,
         text.col=vertice_fill_palette,
         horiz = FALSE,
         inset = c(0.1, 0.1));

}

