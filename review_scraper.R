###################################################
# scraping rum reviews and more from rumratings.com
# data scrape August 21, 2018
###################################################

# --------------------------------------
# Load packages

library(tidyverse)
library(rvest)
library(magrittr)

# -------------------------------------
# Create a list with all top level urls

website     <- "https://www.rumratings.com"
base_url    <- "https://www.rumratings.com/brands?page="

# last page with at least one rating is # 144

page_number <- seq_along(1:144)
page_urls   <- paste0(base_url, page_number)

# ---------------------------------------
# Extract base urls for each rum

brand_base_urls <- list()

k <- 1
for (i in page_urls) {
  
  brand_base_urls[k] <- i              %>% 
    read_html()                        %>%
    html_nodes("div a")                %>%
    html_attr("href")                  %>%
    as.tibble()                        %>%
    filter(grepl("/brands/", value), 
           !grepl("review_id", value)) %>%
    mutate(value = paste0(website, value))
  
  print(k)
  k = k + 1
}

brand_base_urls <- unlist(brand_base_urls)
brand_base_urls <- unique(brand_base_urls)

# ---------------------------------------------
# Extract meta data

name      <- NULL
n_ratings <- NULL
price     <- NULL
country   <- NULL
category  <- NULL

k <- 1
for (url in brand_base_urls){
  
  html <- url               %>%
    read_html() 
  
  # name
  n <- html                 %>%
    html_nodes("title")     %>%
    html_text()             %>%
    str_split("\\|")
  
  name[k] <- str_trim(n[[1]][1])

  # n ratings
  r <- html                 %>% 
       html_nodes("p span") %>% 
       html_text()
  
  n_ratings[k] <- as.numeric(r[2])
    
  # other vars
  other <- html             %>% 
    html_nodes("p a")       %>% 
    html_text()
  
  country[k]  <- other[1]
  category[k] <- other[2]
  price[k]    <- other[3] 
  
  print(k)
  k = k + 1
}

meta_df <- tibble(name, 
                  country, 
                  category, 
                  price = as.numeric(gsub("Best Price \\$","", price)), 
                  n_ratings, 
                  brand_base_urls) # joining variable

# with duplicates

meta_df_original <- meta_df 
write_csv(meta_df_original, "meta_data_full_original.csv") 

# without duplicates

meta_df <- meta_df[!duplicated(meta_df),]
write_csv(meta_df, "meta_data_full.csv")

# --------------------------------------
# Find last pages for all brand urls

# the loop stopped at 25, likely beacuse the remaining rums only have one page
# and therefore no "last" html tag

last_pages <- NULL

k <- 1
for (i in brand_base_urls) {
  last_pages[k] <- i         %>% 
    read_html()              %>%
    html_nodes(".last a")    %>%
    html_attr("href")        %>%
    as.character()           %>%
    str_split("=")           %>%
    map(2)
  
  print(k)
  k = k +1
}

last_pages <- unlist(last_pages)

# need to insert "1" as last page for the remaining pages

last_pages[26:length(brand_base_urls)] <- "1"

last_df <- tibble(base = paste0(brand_base_urls,"?page="),
                  last = last_pages,
                  brand_base_urls)

# --------------------------------------
# Store all review urls

urls <- NULL

k = 1
for (i in seq_along(last_df$last)) {
  for (j in seq(as.numeric(last_df$last[i]))) {
    urls[k] = paste0(last_df$base[i],j)
  } 
}

# --------------------------------------
# Scrape reviews and other variables

review_id       <- NULL
review          <- NULL
rating          <- NULL
other_vars      <- NULL
base            <- NULL


k = 1 #remember to do 1,2,3 again
for (url in urls) {

  html <- url                     %>%
    read_html() 
  
  review_id[[k]] <- html          %>% 
    html_nodes(".review")         %>%
    html_attr("id")
  
  rating[[k]] <- html             %>% 
    html_nodes(".rating")         %>%
    html_text()
  
  review[[k]] <- html             %>%
    html_nodes(".review_content") %>%
    html_text()
  
  other_vars[[k]] <- html         %>% 
    html_nodes(".span7")          %>%
    html_text()
  
  base[[k]] <- rep(url, length(review_id[[k]]))
  
  print(k)
  k = k + 1
}

# --------------------------------------
# Extract other variables

other_vars = unlist(other_vars)

header        <- NULL
date          <- NULL
reviewer      <- NULL
location      <- NULL
rev_n_ratings <- NULL

k = 1
for (i in other_vars) {
  header[k] <- i        %>%
    str_split("\n")     %>%
    map(2)
  date[k] <- i          %>%
    str_split("\n")     %>%
    map(6)
  reviewer[k] <- i      %>%
    str_split("\n")     %>%
    map(9)
  location[k] <- i      %>%
    str_split("\n")     %>%
    map(11)
  rev_n_ratings[k] <- i %>%
    str_split("\n")     %>%
    map(13)
  
  print(k)
  k = k + 1
}

review_df <- tibble(review_id = unlist(review_id),
                    header = unlist(header),
                    review = unlist(review),
                    date = unlist(date),
                    reviewer = unlist(reviewer),
                    location = unlist(location),
                    rev_n_ratings = unlist(rev_n_ratings),
                    rating = as.numeric(unlist(rating)),
                    brand_base_urls = unlist(base)) 

review_df$brand_base_urls %<>% 
  str_split("\\?") %>%
  map(1) %>%
  unlist()

# --------------------------------------
# Join data frames

df <- left_join(review_df, meta_df)

write_csv(df, "big_rum_data_v1.csv")

