library(tidyverse)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)

bayern_nodes <- read_csv("Project/Data/Bayern/nodelist_bayern.csv")

bayern_edges <- read_csv2("Project/Data/Bayern/edgelist_bayern.csv")

bayern_graph <- tbl_graph(nodes = bayern_nodes, edges = bayern_edges, directed = FALSE)

autograph(bayern_graph)

ggraph(bayern_graph, layout = "fr") +
  geom_edge_link0(edge_color = "grey66") + 
  geom_node_text(aes(label = name))

ggraph(bayern_graph, layout = "fr") +
  geom_edge_link0(aes(edge_color = weight)) + 
  geom_node_text(aes(label = name))




                  