######################################################
# map of reviews by country divided by population
######################################################

# ---------------------------------------------------
# Load packages

library(tidyverse)
library(janitor)
library(maps)
library(peachythemes)
library(ggrepel)

# ---------------------------------------------------
# Create ggplot theme

dark_theme <- function() {
  theme_void() +
    theme(panel.background  = element_blank()) +
    theme(plot.background   = element_rect(fill = "#272935")) +      # or #252732
    theme(plot.margin       = unit(c(.5, .5, .5, .5), "cm")) +
    theme(text              = element_text(colour = "white")) +
    theme(axis.text         = element_blank()) +
    #theme(plot.title       = element_text()) +
    theme(strip.text.x      = element_blank()) +
    theme(panel.spacing     = unit(3, "lines")) +
    theme(panel.grid.major  = element_blank()) +
    theme(panel.grid.minor  = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key        = element_blank()) +
    theme(legend.title      = element_blank()) +
    theme(plot.subtitle     = element_text(face="italic")) +
    theme(plot.caption      = element_text(face="italic")) +
    theme(legend.position   = c(0.9,0.08))
}

# ---------------------------------------------------
# Load and clean population data

pop_df         <- read_csv("world_pop.csv")

new_names      <- list("Saint Kitts And Nevis" = "Saint Kitts", "Fidji" = "Fiji",
                  "uk" = "UK", "United Kingdom" = "UK", "United States" = "USA", 
                  "St. Maartin" = "Saint Martin", "Dominica" = "Dominican Republic", 
                  "DK" = "Denmark", "FR" = "France", "Russian Federation" = "Russia", 
                  "Venezuela, Bolivarian Republic of" = "Venezuela","Curaçao" ="Curacao",
                  "CZ" = "Czech Republic","Iran, Islamic Republic Of" = "Iran",
                  "Taiwan, Republic Of China" = "Taiwan", 
                  "DR Congo" = "Democratic Republic of the Congo")

pop_df$country <- dplyr::recode(pop_df$country,!!!new_names)

# ---------------------------------------------------
# Load and prepare n review and location data

review_df    <- read_csv("big_rum_data_v2.csv")

review_table <- review_df    %>% 
  tabyl(location)            %>% 
  select(country = "location", n)

# ---------------------------------------------------
# Load and clean population data, fetch map

world_df <- inner_join(review_table,pop_df) %>%
  mutate(proportion = n / population)

map_df   <- map_data("world")

# unused in this case

#df <- left_join(map_df, world_df, by=c("region" = "country")) %>%
#  as_tibble()

# ---------------------------------------------------
# Create and save world map

ggplot()                                                                        +
  geom_map(data = map_df, map = map_df, 
           aes(map_id = region,x = long, y = lat),
           fill = "#272935", colour = "gray97", size = 0.3)                     +
  geom_map(data = world_df, map = map_df, 
           aes(fill = log(proportion), map_id = country),colour="thistle")      +
  scale_fill_gradient(low = "thistle", high = "darkred",guide=F)                +
  #coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))          +
  coord_equal(ylim = c(-55,90))                                                 +
  labs(title    = "Reviews by country",
       subtitle = "Adjusted for population size",
       caption  = "source: wikipedia.org")                                       +
  dark_theme() 
  
ggsave("world_reviewers.png", width = 10, height = 8)

# ---------------------------------------------------
# Create and save map of Europe with country names

country_names_eu <- map_df %>% 
  group_by(region) %>%
  summarise(m_long=mean(long), m_lat =mean(lat)) %>%
  dplyr::filter(m_long > -11, m_long < 41, m_lat >= 35, m_lat <= 70)

ggplot()                                                                          +
  geom_map(data = map_df, map = map_df, 
           aes(map_id = region,x=long, y=lat),
           fill="#272935", colour = "gray97", size = 0.3)                         +
  geom_map(data = world_df,map = map_df, 
           aes(fill = log(proportion), map_id = country),colour="thistle")        +
  scale_fill_gradient(low = "thistle", high = "darkred",guide=F)                  +
  geom_text_repel(data=country_names_eu, 
            aes(m_long, m_lat, label = region), size=4, colour = "white")         +
  coord_equal(ylim=c(35,70), xlim=c(-10,40))                                      +
  labs(title   = "Reviews by country",
       subtitle = "Adjusted for population size",
       caption = "source: wikipedia.org")                                         +
  dark_theme()

ggsave("euro_reviewers.png", width = 10, height = 10)

# ---------------------------------------------------
# Top 25 reviewer countries (incl territories) above 5

review_pop <- world_df      %>%
  dplyr::filter(n > 5)      %>%
  arrange(desc(proportion)) %>%
  top_n(25)                 %>%
  mutate(order = rev(row_number()))

review_pop                  %>% 
  ggplot(aes(x = order, y = proportion, fill = log(proportion)))    +
  
  geom_col(colour="thistle")                                        +
  scale_x_continuous(breaks = review_pop$order,
                     labels = review_pop$country)                   +
  coord_flip()                                                      +
  scale_fill_gradient(low   = "thistle",
                      high  = "darkred",guide=F) +
  labs(title                = "Top 25 countries contributing reviews", 
       subtitle             = "Adjusted for population size",
       caption              = "Source: wikipedia.org")                 +
  theme(axis.text           = element_text(colour = "white", size = 10),
        text                = element_text(colour = "white"),
        panel.background    = element_blank(),
        plot.background     = element_rect(fill = "#272935"),  # or #252732
        panel.border        = element_blank(),
        panel.grid          = element_blank(),
        axis.title          = element_blank(),
        plot.subtitle       = element_text(face="italic"),
        plot.caption        = element_text(face="italic"))

ggsave("top25_rev_country.png")

# ---------------------------------------------------
# Number of individual reviewers

# n reviewers:
length(unique(review_df$reviewer))

n_reviewers <- review_df     %>% 
   select(location,reviewer) %>%
   distinct()                %>%
   group_by(location)        %>%
   count() %>%
   ungroup()

n_reviewers_df <- inner_join(n_reviewers,pop_df, by=c("location" = "country")) %>%
  mutate(proportion = n / population) %>%
  arrange(desc(proportion))

ggplot()                                                                        +
  geom_map(data = map_df, map = map_df, 
           aes(map_id = region,x = long, y = lat),
           fill = "#272935", colour = "gray97", size = 0.3)                     +
  geom_map(data = n_reviewers_df, map = map_df, 
           aes(fill = log(proportion), map_id = location),colour="thistle")                      +
  scale_fill_gradient(low = "thistle", high = "darkred",guide=F)                +
  #coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))          +
  coord_equal(ylim = c(-55,90))                                                 +
  labs(title    = "Number of reviewers by country",
       subtitle = "Adjusted for population size",
       caption  = "source: wikipedia.org")                                       +
  dark_theme() 

ggsave("world_n_reviewers.png", width = 10, height = 8)

# ---------------------------------------------------
# Create and save map of n reviewers in Europe

ggplot()                                                                        +
  geom_map(data = map_df, map = map_df, 
           aes(map_id = region,x = long, y = lat),
           fill = "#272935", colour = "gray97", size = 0.3)                     +
  geom_map(data = n_reviewers_df, map = map_df, 
           aes(fill = log(proportion), map_id = location),colour="thistle")     +
  scale_fill_gradient(low = "thistle", high = "darkred",guide=F)                +
  #coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))          +
  coord_equal(ylim=c(35,70), xlim=c(-10,40))                                    +
  labs(title    = "Number of reviewers by country",
       subtitle = "Adjusted for population size",
       caption  = "source: wikipedia.org")                                      +
  dark_theme() 

ggsave("europe_n_reviewers.png", width = 10, height = 8)

# ---------------------------------------------------
# Top 25countries (incl territories) for n reviewers above 5

top_n_reviewers <- n_reviewers_df %>%
  dplyr::filter(n > 5)            %>%
  arrange(desc(proportion))       %>%
  top_n(25)                       %>%
  mutate(order = rev(row_number()))

top_n_reviewers                   %>% 
  ggplot(aes(x = order, y = proportion, fill = log(proportion)))      +
  
  geom_col(colour="thistle")                                           +
  scale_x_continuous(breaks = top_n_reviewers$order,
                     labels = top_n_reviewers$location)                +
  coord_flip()                                                         +
  scale_fill_gradient(low   = "thistle",
                      high  = "darkred",guide=F) +
  labs(title                = "Top 25 countries with contributing reviewers", 
       subtitle             = "Adjusted for population size",
       caption              = "Source: wikipedia.org")                 +
  theme(axis.text           = element_text(colour = "white", size = 10),
        text                = element_text(colour = "white"),
        panel.background    = element_blank(),
        plot.background     = element_rect(fill = "#272935"),  # or #252732
        panel.border        = element_blank(),
        panel.grid          = element_blank(),
        axis.title          = element_blank(),
        plot.subtitle       = element_text(face="italic"),
        plot.caption        = element_text(face="italic"))

ggsave("top25_n_reviewers_country.png")

# ---------------------------------------------------
# Sugar by country 

sugar_df <- df                  %>%
  select(name,country,sugar)    %>%
  distinct()                    %>%
  dplyr::filter(!is.na(sugar))  %>%
  group_by(country)             %>%
  mutate(m = mean(sugar),n=n()) %>%
  select(-name, -sugar)         %>%
  dplyr::filter(n > 2)          %>%
  distinct()                    %>%
  arrange(desc(m))

ggplot() +
  geom_map(data=map_df, map=map_df, aes(map_id=region,x=long, y=lat),
           fill="#272935", colour = "gray97", size = 0.3) +
  geom_map(data=sugar_df,map=map_df, aes(fill=m, map_id=country)) +
  scale_fill_gradient(low = "thistle", high = "darkred",guide=F) +
  #coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  coord_equal(ylim=c(-55,90)) +
  labs(title = "Reviews by country / population size",
       caption = "source: wikipedia.org") +
  dark_theme() 

# sugar ~ country boxplots

df                                         %>%
  select(name,country,sugar,category)      %>%
  distinct()                               %>%
  dplyr::filter(!is.na(sugar),category == "Aged") %>%
  group_by(country) %>%
  add_count()                           %>%
  dplyr::filter(n > 3,country!="Caribbean",country!="Italy") %>%
  ggplot(aes(x=country,y=sugar)) +
  geom_boxplot(colour="thistle",fill="darkred") +
  labs(title                = "Sugar content in aged rum by country", 
       subtitle             = "countries with more than four rums with known sugar content") +
  theme(axis.text           = element_text(colour = "white", size = 10),
        text                = element_text(colour = "white"),
        panel.background    = element_blank(),
        plot.background     = element_rect(fill = "#272935"),  # or #252732
        panel.border        = element_blank(),
        panel.grid          = element_blank(),
        axis.title          = element_blank(),
        plot.subtitle       = element_text(face="italic"),
        plot.caption        = element_text(face="italic"))

ggsave("country_sugar_boxplot.png")


sugar_df                  %>% 
  ggplot(aes(x = m, y = proportion, fill = log(proportion)))      +
  
  geom_col(colour="thistle")                                           +
  scale_x_continuous(breaks = top_n_reviewers$order,
                     labels = top_n_reviewers$location)                +
  coord_flip()                                                         +
  scale_fill_gradient(low   = "thistle",
                      high  = "darkred",guide=F) +
  labs(title                = "Top 25 countries with contributing reviewers", 
       subtitle             = "Adjusted for population size",
       caption              = "Source: wikipedia.org")                 +
  theme(axis.text           = element_text(colour = "white", size = 10),
        text                = element_text(colour = "white"),
        panel.background    = element_blank(),
        plot.background     = element_rect(fill = "#272935"),  # or #252732
        panel.border        = element_blank(),
        panel.grid          = element_blank(),
        axis.title          = element_blank(),
        plot.subtitle       = element_text(face="italic"),
        plot.caption        = element_text(face="italic"))

  