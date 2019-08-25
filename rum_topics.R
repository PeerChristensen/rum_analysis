
library(tidyverse)
library(tidytext)
library(stm)
library(magrittr)
library(tm)
library(furrr)
library(scales)
library(cld3)

df    <- read_csv("big_rum_data_v2.csv")

language <- detect_language(df$review)

df <- df %>% 
  mutate(language = language) %>%
  dplyr::filter(language == "en")

df              <- df                      %>%
  unnest_tokens(word, review)              %>%
  anti_join(stop_words)                    %>%
  add_count(word)                          %>%
  dplyr::filter(n > 10)       %>%
  select(-n)


df <- df %>%
  dplyr::filter(!word %in% c("rum","rums"),
                !str_detect(word,"[0-9]"))
  

n_topics = seq(3,5,1) # change to seq

df_sparse <- df                  %>%
  mutate(review_id = factor(review_id)) %>%
  count(review_id, word)          %>%
  cast_dtm(review_id,word,n)

df_sparse = removeSparseTerms(df_sparse, 0.99)

df_sparse <- as.dfm(df_sparse)

many_models_stm <- data_frame(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(df_sparse, K = ., verbose = FALSE)))

heldout <- make.heldout(df_sparse)

k_result <- many_models_stm %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, df_sparse),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, df_sparse),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 15")

topic_model_stm <- k_result %>% 
  dplyr::filter(K ==3)            %>% 
  pull(topic_model)         %>% 
  .[[1]]


td_beta <- tidy(topic_model_stm)

top_terms <- td_beta  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(10, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_stm, matrix = "gamma",
                 document_names = rownames(df_sparse))

gamma_terms <- td_gamma              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot <- gamma_terms %>%
  #top_n(3, gamma)      %>% # n topics
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,width=.7) +
  geom_text(hjust = -.05, vjust=0, size = 4, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.45),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "STM: Top 10 topics by prevalence in the SOU",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot
