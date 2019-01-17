# updated analysis
# July 2018

# LOAD PACKAGES ---------------------------
library(tidyverse)
library(gridExtra)
library(betareg)
library(plotly)
library(magrittr)
library(caret)
library(sjPlot)
library(effects)
library(ggiraph)
library(GGally)
library(gganimate)

# LOAD DATA -------------------------------
df = read_csv2("rum_to_R.csv")
orig_df = df # store for later

# INSPECT DATA -----------------------------

dim(df)
sample_n(df, 5)

grid.arrange(
  # sugar content distribution
  df %>% ggplot(aes(x="",y=Sugar)) + geom_jitter(alpha = .5, colour = "steelblue") +
    geom_boxplot(alpha=.5) +
    geom_hline(yintercept = mean(na.omit(df$Sugar)), colour = "darkred", linetype = "dashed"),
  df %>% ggplot(aes(x=Sugar)) + geom_density() + geom_vline(xintercept = 55, colour = "steelblue"),
  # price distribution
  df %>% ggplot(aes(x="",y=Price)) + geom_jitter(alpha = .5, colour = "steelblue") + 
    geom_boxplot(alpha=.5) +
    geom_hline(yintercept = mean(na.omit(df$Price)), colour = "darkred", linetype = "dashed"),
  df %>% ggplot(aes(x=Price)) + geom_density() + geom_vline(xintercept = 135, colour = "steelblue"),
  # ratings distribution
  df %>% ggplot(aes(x="",y=Rating)) +geom_jitter(alpha = .5, colour = "steelblue") + 
    geom_boxplot(alpha=.5) +
    geom_hline(yintercept = mean(df$Rating), colour = "darkred", linetype = "dashed"),
  df %>% ggplot(aes(x=Rating)) + geom_density() + 
    geom_vline(xintercept = 4, colour = "steelblue"), ncol = 2)

# REMOVE OUTLIERS  AND NAs -----------------------------

original_size = nrow(df)
df %<>% filter(Sugar < 55, Price < 135, Rating >= 4)
new_size = nrow(df)
new_size

original_size-new_size # n data points removed
1-(new_size/original_size)   #prop. removed = .77

# CORRELATIONS -----------------------------

df %>% select(Sugar, Price, Rating) %>% cor()
df %>% select(Sugar, Price, Rating) %>% ggpairs()

grid.arrange(
# Rating ~ Sugar
  df %>% ggplot(aes(x=Sugar, y = Rating)) + 
  geom_point(alpha = .7, aes(size = Raters, colour = Category)) +
  geom_smooth(method = "lm", colour = "black") +
  geom_smooth(method = "loess"), 
# Rating ~ Price
df %>%
  ggplot(aes(x=Price, y = Rating)) +
  geom_jitter(alpha=.7, aes(size = Raters, colour = Category)) +
  geom_smooth(method = "lm", colour = "black") +
  geom_smooth(method = "loess"), ncol=2) 

# SIMPLE REGRESSION: SUGAR --------------------------------

# We're mainly interested in whether Sugar is a significant predictor of rating.
# Let's first perform a univariate analysis on all rums for which the sugar content has been tested

sugar_df = orig_df %>%
  filter(Sugar < 55)
nrow(sugar_df) #n 193
fit_sugar = lm(Rating ~ Sugar, weights = Raters, data = df)
summary(fit_sugar) # Adjusted R-squared:  0.2007, p = 3.89e-11 ***
plot(fit_sugar)

# plot the 
sugar_df %>% ggplot(aes(x=Sugar, y = Rating)) + 
  geom_point(alpha = .7, aes(size = Raters, colour = Category)) +
  geom_smooth(method = "lm", colour = "black") +
  geom_smooth(method = "loess")

# MULTIPLE REGRESSION: PRICE + SUGAR --------------------------------

# We'll control for Price and check if the addition of Sugar can improve the model

fit1 = lm(Rating ~ Price, weights = Raters, data = df)
summary(fit1)
# Adjusted R-squared:  0.5194

fit2 = lm(Rating ~ Price + Sugar, weights = Raters, data = df)

summary(fit2)
# Adjusted R-squared:  0.5943  

anova(fit1, fit2)
AIC(fit1,fit2) # #AIC penalizes the model for including more variables
# fit2 best

# DIAGNOSTICS ----------------------------------------

plot(fit2)
plot_model(fit2, type = "diag")

# BETA REGRESSION I -----------------------------------

df$betaRating = df$Rating/10
fit3 = betareg(betaRating ~ Price + Sugar, weights = Raters, data = df)
summary(fit3)
# Pseudo R-squared: 0.6737

# 5x5 CROSS-VALIDATION I -----------------------------

set.seed(621)

model <- train(
  Rating ~ Price + Sugar, data = df,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", number = 5,
    repeats=5, verboseIter = TRUE))
model
# mean error = 0.5

# REGRESSION MODELS II --------------------------------

#aged only
aged = df %>% filter(Category == "Aged")
  
fit1 = lm(Rating ~ Price, weights = Raters, data = aged)
summary(fit1)
# Adjusted R-squared:  0.50

fit2 = lm(Rating ~ Price + Sugar, weights = Raters, data = aged)
summary(fit2)
# Adjusted R-squared:  0.6291

anova(fit1, fit2)
AIC(fit1,fit2) # #AIC penalizes the model for including more variables
# fit2 best

# BETA REGRESSION II -----------------------------------

aged$betaRating = aged$Rating/10
fit3 = betareg(betaRating ~ Price + Sugar, weights = Raters, data = aged)
summary(fit3)
# Pseudo R-squared: 0.67

# 5x5 CROSS-VALIDATION II -----------------------------

set.seed(621)

model <- train(
  Rating ~ Price + Sugar, data = aged,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", number = 5,
    repeats=5, verboseIter = TRUE))
model
# mean abs. error = 0.34

# 2D INTERACTIVE BUBBLE PLOT -----------------------------------------
# We'll use the original data containing values with sugar content
orig_df$Label2 <- gsub("'", '', orig_df$Label)
p1 = orig_df %>%
  filter(!is.na(Sugar)) %>%
  ggplot(aes(x = Price, y = Rating, size = Raters, colour = Sugar)) +
  geom_point_interactive(aes(tooltip=factor(Label2)), alpha=.8) +
  scale_colour_viridis_c(option = "A", begin=.15) +
  scale_size(range = c(3, 10)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "gray70"),
        panel.grid.minor.y = element_line(colour = "gray70")) +
  ylim(0,10) +
  guides(size = guide_legend(title = "n Raters"))

ggiraph(code = print(p1),width_svg = 10, height_svg = 10)

# 2D ANIMATED BUBBLE PLOT -----------------------------------------

plot_2d = orig_df %>% 
  filter(!is.na(Sugar)) %>%
  ggplot(aes(x = Price, y = Rating, size = Raters, colour = Sugar)) +
  geom_point(alpha = .8) +
  scale_colour_viridis_c(option = "A", begin=.15) +
  scale_size(range = c(5, 15)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "gray70"),
        panel.grid.minor.y = element_line(colour = "gray70")) +
  ylim(0,10) +
  guides(size = guide_legend(title = "n Raters")) +
  labs(title = 'Relationship between rum ratings, price and sugar content\n\nRating: {round(frame_time,1)}') +
  transition_time(round(Rating,1)) +
  ease_aes('linear') +
  shadow_wake(.6)
animate(plot_2d, nframes = 15, fps = 2)

# INTERACTIVE 3D BUBBLE PLOT -----------------------------------------

df %>%
  plot_ly(x = ~Price, y = ~Sugar, z = ~Rating, color = ~Category, size = ~Raters,
             marker = list(symbol = 'circle', sizemode = 'diameter',colorscale = 'Viridis'), sizes = c(10, 25),
             type='scatter3d', mode='markers', hoverinfo = 'text',
             text = ~paste('Label:', Label, '<br>Country:', Country, '<br>Category:', Category,
                           '<br>Rating:', Rating, '<br>Price:', Price,'<br>Sugar:', Sugar)) %>%
  layout(title = 'Relationship between rum ratings, price and sugar content')

# Labels sorted by Rating and Sugar (bars)
orig_df %>%
  filter(!is.na(Sugar)) %>%
  top_n(25,Rating) %>%
  arrange(desc(Rating)) %>%
  ggplot(aes(x = reorder(Label,Rating), y = Rating, fill = Sugar)) +
  geom_col() +
  scale_fill_viridis_c(option = "B", begin = .15) +
  coord_flip(ylim=c(5,10)) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray70"),
        panel.grid.minor.x = element_line(colour = "gray70")) 
  
