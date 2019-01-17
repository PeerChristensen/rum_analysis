###########################################
# scrape data on country population size
# Data from 2018
###########################################

# -----------------------------------------
# load packages

library(tidyverse)
library(rvest)
library(magrittr)

# -----------------------------------------
# scrape data

url <- read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population")

wiki_table <- url                           %>%
  html_table(fill=T)

# -----------------------------------------
# select and clean variables

pop_df <- wiki_table[[2]]                   %>%
  as.tibble()                               %>%
  select(country = starts_with("Country"), 
         population = "Population")         %>%
  mutate(population = as.numeric(gsub(",", "", population)))

pop_df$country                              %<>% 
  str_split(pattern="\\[|\\(")              %>%
  map(1)                                    %>%
  unlist()                                  %>%
  str_trim()

# -----------------------------------------
# write csv-file

write_csv(pop_df, "world_pop.csv")  

