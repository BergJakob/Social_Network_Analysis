### importing files

library(tidyverse)
library(readxl)

books_tsv <- read_tsv("import_training_files/books.tsv",
                      col_types = cols(
                        rank = col_double(),
                        weeks_on_list = col_double(),
                        primary_isbn10 = col_character(),
                        primary_isbn13 = col_double(),
                        publisher = col_character(),
                        title = col_character(),
                        author = col_character(),
                        date = col_date(format = "%d %b %Y")
                      ))

books_txt <- read_delim("import_training_files/books.txt", delim = "|")

ches_2017_modified <- read_csv("import_training_files/ches_2017_modified.csv", skip = 4) %>% 
  pivot_wider(names_from = variable,
              values_from = value)

ches_2017 <- read_csv("import_training_files/ches_2017.csv")

fiction <- read_csv("import_training_files/fiction.csv")

publishers_sheets <- excel_sheets("import_training_files/publishers_with_places.xlsx")
publishers_with_places_a_l <- read_excel("import_training_files/publishers_with_places.xlsx", sheet = publishers_sheets[[1]])
publishers_with_places_m_z <- read_excel("import_training_files/publishers_with_places.xlsx", sheet = publishers_sheets[[2]])
publishers_with_places <- tibble(
  publisher = c(publishers_with_places_a_l$publisher, publishers_with_places_m_z$publisher),
  city = c(publishers_with_places_a_l$city, publishers_with_places_m_z$place)
) %>% 
  separate(city, into = c("city", "state"), sep = ",") # couple of warnings because not all the places are in the US
                                    