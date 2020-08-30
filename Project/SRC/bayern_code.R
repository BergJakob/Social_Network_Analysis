library(tidyverse)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(janitor)

# get data

bayern_nodes <- read_csv("Project/Data/Bayern/nodelist_bayern.csv")

bayern_edges <- read_csv2("Project/Data/Bayern/edgelist_bayern.csv")

bayern_graph <- tbl_graph(nodes = bayern_nodes, edges = bayern_edges, directed = FALSE)

autograph(bayern_graph)

# Netzwerkgraphen, verschiedene Versuche

ggraph(bayern_graph, layout = "fr") +
  geom_edge_link0(edge_color = "grey66") + 
  geom_node_text(aes(label = name))

ggraph(bayern_graph, layout = "fr") +
  geom_edge_link0(aes(edge_color = weight)) + 
  geom_node_text(aes(label = name))

# Zentralisitätsmaße

bayern_degree_weight <- bayern_graph %>% 
  activate(nodes) %>% 
  mutate(degree_weighted = centrality_degree(weights = weight, mode = "total")) %>% 
  as_tibble()

bayern_degree_not_weight <- bayern_graph %>% 
  activate(nodes) %>% 
  mutate(degree_not_weighted = centrality_degree()) %>% 
  as_tibble()

bayern_graph %>% edge_density()

write.csv(bayern_degree_weight, "Project/Data/Bayern/bayern_degree_weight.csv")

write.csv(bayern_degree_not_weight, "Project/Data/Bayern/bayern_degree_not_weight.csv")

kicker_sofascore_grade_bayern <- read_csv2("Project/Data/Bayern/bayern_data.csv") %>%
  clean_names('snake') %>% 
  rename(kicker_grade = kickernote, sofascore_grade = sofa_score_note) %>% 
  select(name, kicker_grade, sofascore_grade)

write.csv(kicker_sofascore_grade_bayern, "Project/Data/Bayern/kicker_sofascore_grade_bayern.csv")

regression_data_degree_both <- left_join(bayern_degree_weight, bayern_degree_not_weight)

write.csv(regression_data_degree_both, "Project/Data/Bayern/regression_data_degree_both.csv")

regression_data_degree_grade <- left_join(regression_data_degree_both, kicker_sofascore_grade_bayern) %>% 
  drop_na()

write.csv(regression_data_degree_grade, "Project/Data/Bayern/regression_data_degree_grade.csv")

data_closeness <- bayern_graph %>% 
  as_tbl_graph() %>% 
  mutate(closeness_centrality = centrality_closeness_harmonic()) %>% 
  as_tibble()

regression_data_degree_grade_closeness <- left_join(regression_data_degree_grade, data_closeness)

write.csv(regression_data_degree_grade_closeness, "Project/Data/Bayern/regression_data_degree_grade_closeness.csv")

# Regression degree - grades

# Kicker, weighted

degree_weighted_kicker_scatterplot_1 <- ggplot(data = regression_data_degree_grade ) +
  geom_point(aes(x = kicker_grade, y = degree_weighted))

degree_weighted_kicker_scatterplot_2 <- ggplot(data = regression_data_degree_grade, aes(x = kicker_grade, 
                                                                                      y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade$kicker_grade, y = regression_data_degree_grade$degree_weighted,
    use = "pairwise.complete.obs")

bivar_reg_kicker_degree_weighted <- lm(kicker_grade ~ degree_weighted, data = regression_data_degree_grade)

summary(bivar_reg_kicker_degree_weighted)

# Kicker, unweighted

degree_not_weighted_kicker_scatterplot_1 <- ggplot(data = regression_data_degree_grade ) +
  geom_point(aes(x = kicker_grade, y = degree_not_weighted))

degree_not_weighted_kicker_scatterplot_2 <- ggplot(data = regression_data_degree_grade, aes(x = kicker_grade, 
                                                                                        y = degree_not_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade$kicker_grade, y = regression_data_degree_grade$degree_not_weighted,
    use = "pairwise.complete.obs")

bivar_reg_kicker_degree_not_weighted <- lm(kicker_grade ~ degree_not_weighted, data = regression_data_degree_grade)

summary(bivar_reg_kicker_degree_not_weighted) 

# Sofascore, weighted

degree_weighted_sofascore_scatterplot_1 <- ggplot(data = regression_data_degree_grade ) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

degree_weighted_sofascore_scatterplot_2 <- ggplot(data = regression_data_degree_grade, aes(x = sofascore_grade, 
                                                                                       y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade$sofascore_grade, y = regression_data_degree_grade$degree_weighted,
    use = "pairwise.complete.obs")

bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_weighted, data = regression_data_degree_grade)

summary(bivar_reg_sofascore_degree_weighted)

# Sofascore, unweighted

degree_weighted_sofascore_scatterplot_1 <- ggplot(data = regression_data_degree_grade ) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

degree_weighted_sofascore_scatterplot_2 <- ggplot(data = regression_data_degree_grade, aes(x = sofascore_grade, 
                                                                                          y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade$sofascore_grade, y = regression_data_degree_grade$degree_weighted,
    use = "pairwise.complete.obs")

bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_weighted, data = regression_data_degree_grade)

summary(bivar_reg_sofascore_degree_weighted)

# Regression closeness - grades

# Kicker

closeness_centrality_kicker_scatterplot_1 <- ggplot(data = regression_data_degree_grade_closeness ) +
  geom_point(aes(x = kicker_grade, y = closeness_centrality))

closeness_centrality_kicker_scatterplot_2 <- ggplot(data = regression_data_degree_grade_closeness, aes(x = kicker_grade, 
                                                                                           y = closeness_centrality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade_closeness$kicker_grade, y = regression_data_degree_grade_closeness$closeness_centrality,
    use = "pairwise.complete.obs")

bivar_reg_kicker_closeness <- lm(kicker_grade ~ closeness_centrality, data = regression_data_degree_grade_closeness)

summary(bivar_reg_kicker_closeness)



                  