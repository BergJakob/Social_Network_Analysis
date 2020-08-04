library(tidyverse)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)

dortmund_nodes <- read_csv("Project/Data/Dortmund/nodelist_dortmund.csv")

dortmund_edges <- read_csv2("Project/Data/Dortmund/edgelist_dortmund.csv")

dortmund_graph <- tbl_graph(nodes = dortmund_nodes, edges = dortmund_edges, directed = FALSE)

autograph(dortmund_graph)

ggraph(dortmund_graph, layout = "fr") +
  geom_edge_link0(edge_color = "grey66") + 
  geom_node_text(aes(label = name))

ggraph(dortmund_graph, layout = "fr") +
  geom_edge_link0(aes(edge_color = weight)) + 
  geom_node_text(aes(label = name))