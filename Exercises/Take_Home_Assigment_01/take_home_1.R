# Take-home Assignment #1 

library(rvest)
library(tidyverse)
url <- "https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018" # url to scrape

# scrape the website
url_html <- read_html(url) # read the page

# extract the HTML table
whole_table <- url_html %>% 
  html_nodes('table') %>% # select table
  html_table(fill = TRUE) %>% # parse it to date frame
  .[[1]] # pull out the data frame

table_content <- whole_table %>%
  select(-X1) %>% # remove redundant column
  filter(!dplyr::row_number() %in% 1:3) # remove redundant rows

raw_headers <- url_html %>%
  html_nodes(".thead-icon") %>% # select element
  html_attr('title') # pull out column names

tidy_bottom_header <- raw_headers[28:length(raw_headers)]

raw_middle_header <- raw_headers[17:27]

tidy_headers <- c( # fix headers
  rep(raw_middle_header[1:7], each = 2),
  "animal_total",
  rep(raw_middle_header[8:length(raw_middle_header)], each = 2),
  "non_animal_total",
  "country_total")

combined_colnames <- paste(tidy_headers, tidy_bottom_header, sep = ';') # create column names
colnames(table_content) <- c("Country", combined_colnames) # add column names
table_content_tbl <- as_tibble(table_content)
table_content_tbl

#3. 

tbl_tidy <- table_content_tbl %>% 
  rename(pork_consump = `Pork;Supplied for Consumption (kg/person/year)`, 
         pork_co2 = `Pork;Kg CO2/person/year`, 
         poultry_consump = `Poultry;Supplied for Consumption (kg/person/year)`,
         poultry_co2 = `Poultry;Kg CO2/person/year`, 
         beef_consump = `Beef;Supplied for Consumption (kg/person/year)`, 
         beef_co2 = `Beef;Kg CO2/person/year`, 
         lamb_goat_consump = `Lamb & Goat;Supplied for Consumption (kg/person/year)`, 
         lamb_goat_co2 = `Lamb & Goat;Kg CO2/person/year`, 
         fish_consump = `Fish;Supplied for Consumption (kg/person/year)`,
         fish_co2 = `Fish;Kg CO2/person/year`, 
         eggs_consump = `Eggs;Supplied for Consumption (kg/person/year)`, 
         eggs_co2 = `Eggs;Kg CO2/person/year`,
         milk_cheese_consump = `Milk - inc. cheese;Supplied for Consumption (kg/person/year)`, 
         milk_cheese_co2 = `Milk - inc. cheese;Kg CO2/person/year`,
         wheat_wheat_products_consump = `Wheat and Wheat Products;Supplied for Consumption (kg/person/year)`,
         wheat_wheat_co2 = `Wheat and Wheat Products;Kg CO2/person/year`,
         rice_consump = `Rice;Supplied for Consumption (kg/person/year)`,
         rice_co2 = `Rice;Kg CO2/person/year`,
         soybeans_consump = `Soybeans;Supplied for Consumption (kg/person/year)`,
         soybeans_co2 = `Soybeans;Kg CO2/person/year`,
         nuts_peanut_butter_consump = `Nuts inc. Peanut Butter;Supplied for Consumption (kg/person/year)`,
         nuts_peanut_butter_co2 = `Nuts inc. Peanut Butter;Kg CO2/person/year`, 
         animal_total_co2 = `animal_total;Kg CO2/person/year`,
         non_animal_total_co2 = `non_animal_total;Kg CO2/person/year`,
         country_total_co2 = `country_total;Kg CO2/person/year`,
         country = Country)

glimpse(tbl_tidy)

 


ggplot(data = tbl_tidy) +
  geom_point(aes(x = fish_co2, y = eggs_consump, color = country))











































