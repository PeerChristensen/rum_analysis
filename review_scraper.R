#################################
# scraping rum reviews and more from rumratings.com
#################################

library(tidyverse)
library(rvest)
library(magrittr)

base_url <- "https://www.rumratings.com/brands?page="

# last page with at least one rating is # 144
page_number <- seq_along(1:144)
page_urls   <- paste0(base_url, page_number)

brand_base_urls <- list()
k <- 1

for (i in page_urls) {
  brand_base_urls[k] <- i %>% 
    read_html() %>%
    html_nodes("div a") %>%
    html_attr("href") %>%
    as.tibble() %>%
    filter(grepl("/brands/", value), !grepl("review_id", value)) %>%
    mutate(value = paste0("https://www.rumratings.com", value))
  k = k + 1
}

brand_base_urls <- unlist(brand_base_urls)
#brand_base_urls = brand_base_urls[1:1000]
################################################
# meta data : name , n ratings, price, country
################################################

name      <- NULL
n_ratings <- NULL
price     <- NULL
country   <- NULL
category  <- NULL
i <- 1

for (url in brand_base_urls){
  
  html <- url %>%
    read_html() 
  
  # name
  n <- html                %>%
    html_nodes("title") %>%
    html_text()         %>%
    str_split("\\|")
  
  name[i] <- str_trim(n[[1]][1])

  # n ratings
  r <- html                 %>% 
       html_nodes("p span") %>% 
       html_text()
  
  n_ratings[i] <- as.numeric(r[2])
    
  # other data
  meta        <- html %>% 
    html_nodes("p a") %>% 
    html_text()
  
  country[i]  <- meta[1]
  category[i] <- meta[2]
  price[i]    <- meta[3] 
  
  i <- i + 1
}

meta_df <- tibble(name, country, category, price, n_ratings, brand_base_urls) %>%
  mutate(price = as.numeric(gsub("Best Price \\$","", price)))


################################################
# reviews, ratings and reviewer
################################################

# 1. find last pages for all brand urls

last_pages <- NULL
k          <- 1
for (i in brand_base_urls) {
  last_pages[k] <- i %>% 
    read_html() %>%
    html_nodes(".last a") %>%
    html_attr("href") %>%
    as.character() %>%
    str_split("=") %>%
    map(2)
  
  k = k +1
}

last_pages <- unlist(last_pages)

last_df = data_frame(
  base = paste0(brand_base_urls,"?page="),
  last = last_pages,
  brand_base_urls)


# 2. store all review urls

all_pages <- NULL
k = 1
for (i in seq_along(last_df$last)) {
  for (j in seq(as.numeric(last_df$last[i]))) {
    all_pages[k] = paste0(last_df$base[i],j)
    k = k + 1
  } 
}

urls = all_pages[1:10]

review_id       <- NULL
review          <- NULL
rating          <- NULL
other_vars      <- NULL
base            <- NULL

# for loop

k = 1
for (url in urls) {

  html <- url %>%
    read_html() 
  
  review_id[[k]] <- html    %>% 
    html_nodes(".review") %>%
    html_attr("id")
  
  rating[[k]] <- html    %>% 
    html_nodes(".rating") %>%
    html_text()
  
  review[[k]] <- html %>%
    html_nodes(".review_content") %>%
    html_text()
  
  other_vars[[k]] <- html %>% 
    html_nodes(".span7") %>%
    html_text()
  
  base[[k]] = rep(url, length(review_id[[k]]))
  
  k = k + 1
}

other_vars = unlist(other_vars)

header   <- NULL
date     <- NULL
reviewer <- NULL
location <- NULL
rev_n_ratings <- NULL

k = 1
for (i in other_vars) {
  header[k] <- i %>%
    str_split("\n") %>%
    map(2)
  date[k] <- i %>%
    str_split("\n") %>%
    map(6)
  reviewer[k] <- i %>%
    str_split("\n") %>%
    map(9)
  location[k] <- i %>%
    str_split("\n") %>%
    map(11)
  rev_n_ratings[k] <- i %>%
    str_split("\n") %>%
    map(13)
  
  k = k + 1
}

review_df <- tibble(
  review_id = unlist(review_id),
  header = unlist(header),
  review = unlist(review),
  date = unlist(date),
  reviewer = unlist(reviewer),
  location = unlist(location),
  rev_n_ratings = unlist(rev_n_ratings),
  rating = as.numeric(unlist(rating)),
  brand_base_urls = unlist(base)
) 

review_df$brand_base_urls %<>% 
  str_split("\\?") %>%
  map(1) %>%
  unlist()

# join tibbles
df <- left_join(review_df, meta_df)

write_csv(df, "big_rum_data_v1.csv")


