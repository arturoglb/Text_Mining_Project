source(here::here("script/setup.R"))
source(here::here("script/sentiment_analysis/sentiment_function.R"))
source(here::here("script/sentiment_analysis/value_function.R"))

# # This analysis doesn't use the tidyverse/quanteda package for sentiments. It
# # returns positive, negative and neg_pos values. We prefer working with more 
# # granular classification/valuation from tidytext ("nrc", "afinn") and
# # sentimentr packages

# # Read data
apple <- read.csv(here::here("data/Apple.csv"))
samsung <- read.csv(here::here("data/Samsung.csv"))
reviews <- read.csv(here::here("data/smartphone_reviews.csv"))

# # Unnest tokens
apple_token <- apple %>% 
  mutate(review_id = seq(1:nrow(apple))) %>% 
  relocate(review_id, .before = "Brand") %>% 
  unnest_tokens(output = "word",
                 input = "Reviews",
                 to_lower = TRUE,
                 strip_punct = TRUE,
                 strip_numeric = TRUE)

# # Get sentiment analysis
# Sentiment based using "nrc" dictionary
apple_sentiment <- sentiment_function(apple_token, "review_id", "sentiment")

ggplot(tibble(sentiment = names(colSums(apple_sentiment)),
              sum_value = colSums(apple_sentiment)),
       aes(x = reorder(sentiment, -sum_value), y = sum_value)) +
  geom_col()

# Value based using "afinn" dictionary and sentimentr
apple_value <- value_function(apple$Reviews, apple_token, review_id, value)

ggplot(melt(apple_value, id.vars = "review_id"), aes(y = value, fill = variable)) +
  geom_boxplot() +
  ylab("Average sentiment value") +
  ggtitle("Sentiment distribution") + 
  guides(fill=guide_legend("Sentiment method"))



