library(tidyverse)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(janitor)

#LM IV DV TAUSCHEN!!!!!

# get data

dortmund_nodes <- read_csv("Project/Data/Dortmund/nodelist_dortmund.csv")

dortmund_edges <- read_csv2("Project/Data/Dortmund/edgelist_dortmund.csv")

dortmund_graph <- tbl_graph(nodes = dortmund_nodes, edges = dortmund_edges, directed = FALSE)

autograph(dortmund_graph)

# Netzwerkgraphen, verschiedene Versuche

ggraph(dortmund_graph, layout = "fr") +
  geom_edge_link0(edge_color = "grey66") + 
  geom_node_text(aes(label = name))

ggraph(dortmund_graph, layout = "fr") +
  geom_edge_link0(aes(edge_color = weight)) + 
  geom_node_text(aes(label = name))

# Zentralisitätsmaße

dortmund_degree_weight <- dortmund_graph %>% 
  activate(nodes) %>% 
  mutate(degree_weighted = centrality_degree(weights = weight, mode = "total")) %>% 
  as_tibble()

dortmund_degree_not_weight <- dortmund_graph %>% 
  activate(nodes) %>% 
  mutate(degree_not_weighted = centrality_degree()) %>% 
  as_tibble()

dortmund_graph %>% edge_density()

write.csv(dortmund_degree_weight, "Project/Data/Dortmund/dortmund_degree_weight.csv")

write.csv(dortmund_degree_not_weight, "Project/Data/Dortmund/dortmund_degree_not_weight.csv")

kicker_sofascore_grade_dortmund <- read_csv2("Project/Data/Dortmund/dortmund_data.csv") %>%
  clean_names('snake') %>% 
  rename(kicker_grade = kickernote, sofascore_grade = sofa_score_note) %>% 
  select(name, kicker_grade, sofascore_grade)

write.csv(kicker_sofascore_grade_dortmund, "Project/Data/Dortmund/kicker_sofascore_grade_dortmund.csv")

regression_data_degree_both <- left_join(dortmund_degree_weight, dortmund_degree_not_weight)

write.csv(regression_data_degree_both, "Project/Data/Dortmund/regression_data_degree_both.csv")

regression_data_degree_grade <- left_join(regression_data_degree_both, kicker_sofascore_grade_dortmund) %>% 
  drop_na()

write.csv(regression_data_degree_grade, "Project/Data/Dortmund/regression_data_degree_grade.csv")

data_closeness <- dortmund_graph %>% 
  as_tbl_graph() %>% 
  mutate(closeness_centrality = centrality_closeness_harmonic()) %>% 
  as_tibble()

regression_data_degree_grade_closeness <- left_join(regression_data_degree_grade, data_closeness)

write.csv(regression_data_degree_grade_closeness, "Project/Data/Dortmund/regression_data_degree_grade_closeness.csv")

# Regression Degree - grades

# Kicker, weighted

degree_weighted_kicker_scatterplot_1 <- ggplot(data = regression_data_degree_grade ) +
  geom_point(aes(x = kicker_grade, y = degree_weighted))

degree_weighted_kicker_scatterplot_2 <- ggplot(data = regression_data_degree_grade,aes(x = kicker_grade, 
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

degree_not_weighted_kicker_scatterplot_2 <- ggplot(data = regression_data_degree_grade,aes(x = kicker_grade, 
                                                                                           y = degree_not_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade$kicker_grade, y = regression_data_degree_grade$degree_not_weighted,
    use = "pairwise.complete.obs")

bivar_reg_kicker_degree_not_weighted <- lm(kicker_grade ~degree_not_weighted, data = regression_data_degree_grade)

summary(bivar_reg_kicker_degree_not_weighted)

# Sofascore, weighted

degree_weighted_sofascore_scatterplot_1 <- ggplot(data = regression_data_degree_grade ) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

degree_weighted_sofascore_scatterplot_2 <- ggplot(data = regression_data_degree_grade,aes(x = sofascore_grade, 
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

degree_weighted_sofascore_scatterplot_2 <- ggplot(data = regression_data_degree_grade,aes(x = sofascore_grade, 
                                                                                          y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade$sofascore_grade, y = regression_data_degree_grade$degree_weighted,
    use = "pairwise.complete.obs")

bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_not_weighted, data = regression_data_degree_grade)

summary(bivar_reg_sofascore_degree_weighted)

# Regression closeness - grades

# Kicker

closeness_centrality_kicker_scatterplot_1 <- ggplot(data = regression_data_degree_grade_closeness) +
  geom_point(aes(x = kicker_grade, y = closeness_centrality))

closeness_centrality_kicker_scatterplot_2 <- ggplot(data = regression_data_degree_grade_closeness, aes(x = kicker_grade, 
                                                                                                       y = closeness_centrality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = regression_data_degree_grade_closeness$kicker_grade, y = regression_data_degree_grade_closeness$closeness_centrality,
    use = "pairwise.complete.obs")

bivar_reg_kicker_closeness <- lm(kicker_grade ~ closeness_centrality, data = regression_data_degree_grade_closeness)

summary(bivar_reg_kicker_closeness)




























