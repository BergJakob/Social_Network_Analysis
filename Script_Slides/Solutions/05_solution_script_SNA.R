# solution factors -- order it alphabetically

library(tidyverse)

election_data <- read_csv("scripts/data/pres16results.csv") %>% 
  drop_na() %>% 
  glimpse()

election_data_w_fct <- election_data %>% 
  mutate(county = as_factor(county),
         candidate = as_factor(cand),
         state = as_factor(st),
         lead = as_factor(lead)) %>% 
  select(county, candidate, state, pct_report:pct, lead)

# fct_reorder state alphabetically
# fct_reorder takes a numeric vector as argument and orders the factor according to it (from highest to lowest value)
# hence, the state, which comes first if ordered alphabetically, needs the highest value
# I achieve this by, first, creating a tibble consisting of the states' names (stored in a character vector) in the election data tibble
# Then I arrange it descendingly (hence, the states whose names are further back in the alphabet come first)
# Thereafter, I add a column named order with the numbers 1:nrow(tibble)
# I left_join the order column
# I use the order column to rearrange it (--> can be dropped afterwards)

order_tbl <- tibble(
  state = unique(as.character(election_data_w_fct$state))
) %>% 
  arrange(desc(state)) %>% 
  mutate(order = 1:length(.$state))

election_data_w_fct %>% 
  left_join(order_tbl) %>% 
  mutate(state = fct_reorder(state, order)) %>% 
  group_by(state) %>% 
  summarize(sum_votes = sum(votes)) %>% 
  ggplot(aes(x = sum_votes, y = state)) +
    geom_point()
