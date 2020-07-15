library(tidygraph)
library(tidyverse)
library(ggraph)

# read in data + tbl_graph (#1)
abortion_nodes <- read_csv("sna-scripts/data/abortion-bills/nodelist_abortion_bills.csv",
                           col_types = cols(
                             id = col_character(), # node names need to be characters
                             female = col_double(),
                             democrat = col_double()
))
abortion_edges <- read_csv("sna-scripts/data/abortion-bills/edgelist_abortion_bills.csv",
                           col_types = cols(
                             from = col_character(), # node names need to be characters
                             to = col_character(), # node names need to be characters
                             weight = col_double()
))

abortion_net <- tbl_graph(nodes = abortion_nodes,
                          edges = abortion_edges)

# 2 
abortion_edges %>% 
  group_by(from, to) %>% 
  summarize(weight = sum(weight))

# 3
abortion_net_classified <- abortion_net %>% 
  activate(edges) %>% # because I add a column to the edgelist
  mutate(classification = case_when(.N()$democrat[from] == .N()$democrat[to] ~ "intra",
                                        .N()$democrat[from] != .N()$democrat[to] ~ "inter"))

# 4
graph_dem <- abortion_net %>% 
  filter(democrat == 1)
graph_rep <- abortion_net %>% 
  filter(democrat == 0)

# 5
# No, it does not. The new network does not have any inter-party edges.
bind_graphs(graph_dem, graph_rep) %>% 
  ggraph() +
  geom_edge_link0() +
  geom_node_point(aes(color = as_factor(democrat)))

