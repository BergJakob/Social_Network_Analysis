library(tidyverse)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(janitor)

# get data

## Bayern

bayern_nodes <- read_csv("Project/Data/Bayern/Raw/nodelist_bayern.csv")

bayern_edges <- read_csv2("Project/Data/Bayern/Raw/edgelist_bayern.csv")

bayern_graph <- tbl_graph(nodes = bayern_nodes, edges = bayern_edges, directed = FALSE)

autograph(bayern_graph)

## Dortmund

dortmund_nodes <- read_csv("Project/Data/Dortmund/Raw/nodelist_dortmund.csv")

dortmund_edges <- read_csv2("Project/Data/Dortmund/Raw/edgelist_dortmund.csv")

dortmund_graph <- tbl_graph(nodes = dortmund_nodes, edges = dortmund_edges, directed = FALSE)

autograph(dortmund_graph)

# Maße

## Dichte

### Bayern 

bayern_graph %>% edge_density()

### Dortmund

dortmund_graph %>% edge_density()

## Degree

### Bayern

bayern_degree_weight <- bayern_graph %>% 
  activate(nodes) %>% 
  mutate(degree_weighted = centrality_degree(weights = weight, mode = "total")) %>% 
  as_tibble()

bayern_degree_not_weight <- bayern_graph %>% 
  activate(nodes) %>% 
  mutate(degree_not_weighted = centrality_degree()) %>% 
  as_tibble()

### Dortmund

dortmund_degree_weight <- dortmund_graph %>% 
  activate(nodes) %>% 
  mutate(degree_weighted = centrality_degree(weights = weight, mode = "total")) %>% 
  as_tibble()

dortmund_degree_not_weight <- dortmund_graph %>% 
  activate(nodes) %>% 
  mutate(degree_not_weighted = centrality_degree()) %>% 
  as_tibble()

## Closeness

### Bayern 

data_closeness_bayern <- bayern_graph %>% 
  as_tbl_graph() %>% 
  mutate(closeness_centrality = centrality_closeness_harmonic()) %>% 
  as_tibble()

### Dortmund

data_closeness_dortmund <- dortmund_graph %>% 
  as_tbl_graph() %>% 
  mutate(closeness_centrality = centrality_closeness_harmonic()) %>% 
  as_tibble()

# merge and save dataframes

## Bayern

write.csv(bayern_degree_weight, "Project/Data/Bayern/Wrangled/bayern_degree_weight.csv")

write.csv(bayern_degree_not_weight, "Project/Data/Bayern/Wrangled/bayern_degree_not_weight.csv")

kicker_sofascore_grade_bayern <- read_csv2("Project/Data/Bayern/Raw/bayern_grades_data.csv") %>%
  clean_names('snake') %>% 
  rename(kicker_grade = kickernote, sofascore_grade = sofa_score_note) %>% 
  select(name, kicker_grade, sofascore_grade)

write.csv(kicker_sofascore_grade_bayern, "Project/Data/Bayern/Wrangled/kicker_sofascore_grade_bayern.csv")

regression_data_degree_both <- left_join(bayern_degree_weight, bayern_degree_not_weight)

write.csv(regression_data_degree_both, "Project/Data/Bayern/Wrangled/regression_data_degree_both.csv")

regression_data_degree_grade <- left_join(regression_data_degree_both, kicker_sofascore_grade_bayern) %>% 
  drop_na()

write.csv(regression_data_degree_grade, "Project/Data/Bayern/Wrangled/regression_data_degree_grade.csv")

data_SNA_2020_regression_bayern <- left_join(regression_data_degree_grade, data_closeness_bayern) %>% 
  select(name, degree_weighted, degree_not_weighted, closeness_centrality, kicker_grade, sofascore_grade)

write.csv(data_SNA_2020_regression_bayern, "Project/Data/Bayern/Wrangled/data_SNA_2020_regression_bayern.csv")

## Dortmund

write.csv(dortmund_degree_weight, "Project/Data/Dortmund/Wrangled/dortmund_degree_weight.csv")

write.csv(dortmund_degree_not_weight, "Project/Data/Dortmund/Wrangled/dortmund_degree_not_weight.csv")

kicker_sofascore_grade_dortmund <- read_csv2("Project/Data/Dortmund/Raw/dortmund_grades_data.csv") %>%
  clean_names('snake') %>% 
  rename(kicker_grade = kickernote, sofascore_grade = sofa_score_note) %>% 
  select(name, kicker_grade, sofascore_grade)

write.csv(kicker_sofascore_grade_dortmund, "Project/Data/Dortmund/Wrangled/kicker_sofascore_grade_dortmund.csv")

regression_data_degree_both <- left_join(dortmund_degree_weight, dortmund_degree_not_weight)

write.csv(regression_data_degree_both, "Project/Data/Dortmund/Wrangled/regression_data_degree_both.csv")

regression_data_degree_grade <- left_join(regression_data_degree_both, kicker_sofascore_grade_dortmund) %>% 
  drop_na()

write.csv(regression_data_degree_grade, "Project/Data/Dortmund/Wrangled/regression_data_degree_grade.csv")

data_SNA_2020_regression_dortmund <- left_join(regression_data_degree_grade, data_closeness_dortmund)

write.csv(data_SNA_2020_regression_dortmund, "Project/Data/Dortmund/Wrangled/data_SNA_2020_regression_dortmund.csv")

# Netzwerkgraph

## Bayern

ggraph(bayern_graph, layout = "fr") +
  geom_edge_link0(aes(edge_color = weight)) + 
  geom_node_text(aes(label = name))

## Dortmund

ggraph(dortmund_graph, layout = "fr") +
  geom_edge_link0(aes(edge_color = weight)) + 
  geom_node_text(aes(label = name))

# Regression Degree - grades

## Bayern

# Kicker, weighted

bayern_degree_weighted_kicker_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_bayern) +
  geom_point(aes(x = kicker_grade, y = degree_weighted))

bayern_degree_weighted_kicker_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_bayern, aes(x = kicker_grade, 
                                                                                                  y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_bayern$kicker_grade, y = data_SNA_2020_regression_bayern$degree_weighted,
    use = "pairwise.complete.obs")

bayern_bivar_reg_kicker_degree_weighted <- lm(kicker_grade ~ degree_weighted, data = data_SNA_2020_regression_bayern)

summary(bayern_bivar_reg_kicker_degree_weighted)

# Kicker, unweighted

bayern_degree_not_weighted_kicker_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_bayern) +
  geom_point(aes(x = kicker_grade, y = degree_not_weighted))

bayern_degree_not_weighted_kicker_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_bayern, aes(x = kicker_grade, 
                                                                                                      y = degree_not_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_bayern$kicker_grade, y = data_SNA_2020_regression_bayern$degree_not_weighted,
    use = "pairwise.complete.obs")

bayern_bivar_reg_kicker_degree_not_weighted <- lm(kicker_grade ~ degree_not_weighted, data = data_SNA_2020_regression_bayern)

summary(bayern_bivar_reg_kicker_degree_not_weighted) 

# Sofascore, weighted

bayern_degree_weighted_sofascore_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_bayern) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

bayern_degree_weighted_sofascore_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_bayern, aes(x = sofascore_grade, 
                                                                                                     y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_bayern$sofascore_grade, y = data_SNA_2020_regression_bayern$degree_weighted,
    use = "pairwise.complete.obs")

bayern_bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_weighted, data = data_SNA_2020_regression_bayern)

summary(bayern_bivar_reg_sofascore_degree_weighted)

# Sofascore, unweighted

bayern_degree_weighted_sofascore_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_bayern) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

bayern_degree_weighted_sofascore_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_bayern, aes(x = sofascore_grade, 
                                                                                                     y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_bayern$sofascore_grade, y = data_SNA_2020_regression_bayern$degree_weighted,
    use = "pairwise.complete.obs")

bayern_bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_weighted, data = data_SNA_2020_regression_bayern)

summary(bayern_bivar_reg_sofascore_degree_weighted)

## Dortmund

# Kicker, weighted

dortmund_degree_weighted_kicker_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_dortmund) +
  geom_point(aes(x = kicker_grade, y = degree_weighted))

dortmund_degree_weighted_kicker_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_dortmund,aes(x = kicker_grade, 
                                                                                                     y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_dortmund$kicker_grade, y = data_SNA_2020_regression_dortmund$degree_weighted,
    use = "pairwise.complete.obs")

dortmund_bivar_reg_kicker_degree_weighted <- lm(kicker_grade ~ degree_weighted, data = data_SNA_2020_regression_dortmund)

summary(dortmund_bivar_reg_kicker_degree_weighted)

# Kicker, unweighted

dortmund_degree_not_weighted_kicker_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_dortmund) +
  geom_point(aes(x = kicker_grade, y = degree_not_weighted))

dortmund_degree_not_weighted_kicker_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_dortmund,aes(x = kicker_grade, 
                                                                                                         y = degree_not_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_dortmund$kicker_grade, y = data_SNA_2020_regression_dortmund$degree_not_weighted,
    use = "pairwise.complete.obs")

dortmund_bivar_reg_kicker_degree_not_weighted <- lm(kicker_grade ~ degree_not_weighted, data = data_SNA_2020_regression_dortmund)

summary(dortmund_bivar_reg_kicker_degree_not_weighted)

# Sofascore, weighted

dortmund_degree_weighted_sofascore_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_dortmund) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

dortmund_degree_weighted_sofascore_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_dortmund,aes(x = sofascore_grade, 
                                                                                                        y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_dortmund$sofascore_grade, y = data_SNA_2020_regression_dortmund$degree_weighted,
    use = "pairwise.complete.obs")

dortmund_bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_weighted, data = data_SNA_2020_regression_dortmund)

summary(dortmund_bivar_reg_sofascore_degree_weighted)

# Sofascore, unweighted

dortmund_degree_weighted_sofascore_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_dortmund) +
  geom_point(aes(x = sofascore_grade, y = degree_weighted))

dortmund_degree_weighted_sofascore_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_dortmund,aes(x = sofascore_grade, 
                                                                                                        y = degree_weighted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_dortmund$sofascore_grade, y = data_SNA_2020_regression_dortmund$degree_weighted,
    use = "pairwise.complete.obs")

dortmund_bivar_reg_sofascore_degree_weighted <- lm(sofascore_grade ~ degree_not_weighted, data = data_SNA_2020_regression_dortmund)

summary(dortmund_bivar_reg_sofascore_degree_weighted)

# Regression closeness - grades

## Bayern

bayern_closeness_centrality_kicker_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_bayern) +
  geom_point(aes(x = kicker_grade, y = closeness_centrality))

bayern_closeness_centrality_kicker_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_bayern, aes(x = kicker_grade, 
                                                                                                       y = closeness_centrality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_bayern$kicker_grade, y = data_SNA_2020_regression_bayern$closeness_centrality,
    use = "pairwise.complete.obs")

bayern_bivar_reg_kicker_closeness <- lm(kicker_grade ~ closeness_centrality, data = data_SNA_2020_regression_bayern)

summary(bayern_bivar_reg_kicker_closeness)

# Sofascore

bayern_closeness_centrality_sofascore_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_bayern) +
  geom_point(aes(x = sofascore_grade, y = closeness_centrality))

bayern_closeness_centrality_sofascore_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_bayern, aes(x = sofascore_grade, 
                                                                                                          y = closeness_centrality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_bayern$sofascore_grade, y = data_SNA_2020_regression_bayern$closeness_centrality,
    use = "pairwise.complete.obs")

bayern_bivar_reg_sofascore_closeness <- lm(sofascore_grade ~ closeness_centrality, data = data_SNA_2020_regression_bayern)

summary(bayern_bivar_reg_sofascore_closeness)

## Dortmund

dortmund_closeness_centrality_kicker_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_dortmund) +
  geom_point(aes(x = kicker_grade, y = closeness_centrality))

dortmund_closeness_centrality_kicker_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_dortmund, aes(x = kicker_grade, 
                                                                                                           y = closeness_centrality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_dortmund$kicker_grade, y = data_SNA_2020_regression_dortmund$closeness_centrality,
    use = "pairwise.complete.obs")

dortmund_bivar_reg_kicker_closeness <- lm(kicker_grade ~ closeness_centrality, data = data_SNA_2020_regression_dortmund)

summary(dortmund_bivar_reg_kicker_closeness)

# Sofascore

dortmund_closeness_centrality_sofascore_scatterplot_1 <- ggplot(data = data_SNA_2020_regression_dortmund) +
  geom_point(aes(x = sofascore_grade, y = closeness_centrality))

dortmund_closeness_centrality_sofascore_scatterplot_2 <- ggplot(data = data_SNA_2020_regression_dortmund, aes(x = sofascore_grade, 
                                                                                                              y = closeness_centrality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(x = data_SNA_2020_regression_dortmund$sofascore_grade, y = data_SNA_2020_regression_dortmund$closeness_centrality,
    use = "pairwise.complete.obs")

dortmund_bivar_reg_sofascore_closeness <- lm(sofascore_grade ~ closeness_centrality, data = data_SNA_2020_regression_dortmund)

summary(dortmund_bivar_reg_sofascore_closeness)




