# clean up rum data

library(tidyverse)
library(magrittr)
library(lubridate)

df <- read_csv("big_rum_data_v1.csv")

df$review_id %<>% 
  str_split("_") %>%
  map(2) %>%
  unlist()

df$date = dmy(df$date)

df$review %<>% str_remove_all("\n")

df$rev_n_ratings = as.numeric(gsub(c(" ratings|rating"),"", df$rev_n_ratings))

write_csv(df, "big_rum_data_v2.csv")


