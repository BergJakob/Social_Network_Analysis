library(tidyverse)
library(tidygraph)
library(igraph)

test_nodes <- read.csv("Project/test_nodes.csv")

test_edges <- read_csv("Project/test_edges.csv") %>% 
  separate('from;to', into = c("from", "to"), sep = ";")

test_graph <- tbl_graph(nodes = test_nodes, edges = test_edges, directed = FALSE)



