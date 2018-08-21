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

df$rev_n_ratings <- as.numeric(gsub(" ratings|rating"),"", df$rev_n_ratings)

# -------------------------
# add sugar data

sugar_df <- read_csv2("rum_to_r.csv")

sugar_df %<>% rename("name"=Label, "sugar"=Sugar)

df <- left_join(df, sugar_df)

# -------------------------
# write new data file

write_csv(df, "big_rum_data_v2.csv")

# df = read_csv("big_rum_data_v2.csv")

