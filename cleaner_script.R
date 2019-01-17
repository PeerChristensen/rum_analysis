##########################
# clean up rum data
##########################

# ------------------------
# load packages

library(tidyverse)
library(magrittr)
library(lubridate)

# -------------------------
# load data

df <- read_csv("big_rum_data_v1.csv")

# -------------------------
# clean data

df$review_id      %<>% 
  str_split("_")  %>%
  map(2)          %>%
  unlist()

df$date = dmy(df$date)

df$review         %<>% 
  str_remove_all("\n")

df$rev_n_ratings <- as.numeric(gsub(" ratings|rating","", df$rev_n_ratings))

# clean country and location !

df$country <- factor(df$country)
df$location <- factor(df$location)

new_names <- list("Saint Kitts And Nevis" = "Saint Kitts", "Fidji" = "Fiji",
                  "uk" = "UK", "United Kingdom" = "UK", "United States" = "USA", 
                  "St. Maartin" = "Saint Martin", "Dominica" = "Dominican Republic", "DK" = "Denmark",
                  "FR" = "France", "Russian Federation" = "Russia", "Venezuela, Bolivarian Republic of" = "Venezuela",
                  "Curaçao" ="Curacao", "CZ" = "Czech Republic","Iran, Islamic Republic Of" = "Iran",
                  "Taiwan, Republic Of China" = "Taiwan", "DR Congo" = "Democratic Republic of the Congo")

df$country <- dplyr::recode(df$country,!!!new_names)
df$location <- dplyr::recode(df$location,!!!new_names)


# -------------------------
# add sugar data

sugar_df <- read_csv2("rum_to_r.csv")

sugar_df               %<>% 
  select(Label, Sugar) %>% 
  rename("name"=Label, "sugar"=Sugar)

df <- left_join(df, sugar_df)

# -------------------------
# write new data file

write_csv(df, "big_rum_data_v2.csv")

# df = read_csv("big_rum_data_v2.csv")

