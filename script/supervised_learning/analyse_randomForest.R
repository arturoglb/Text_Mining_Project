source(here::here("script/setup.R"))

# # Load pre-trained random forest object
load(here::here("script/models/randomForest/rf_1000.RData"))

# # Read and join embedded document matrix with sentimentr score per document
sentimentr_documents <- read.csv(here::here("script/sentiment_analysis/sentiment_data/sentimentr_per_review.csv")) %>% 
  select(review_id, brand = Brand, model = Model, sentimentr_score = sentimentr_value)

embedded_documents <- select(read.csv(here::here("script/unsupervised_learning/embedding/embedding_data/embedded_documents.csv")), -X) %>%
  mutate(review_id = as.integer(rownames(.))) %>% 
  relocate(review_id, .before = V1)

# # Training and testing dataset
dataset <- left_join(embedded_documents, sentimentr_documents,  by = "review_id") %>% 
  select(-review_id)
dataset$brand <- as.factor(dataset$brand)
dataset$model <- as.factor(dataset$model)

# Split the data
set.seed(456)
split <- sample.split(dataset, SplitRatio = 0.75)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")

# Evaluate model accuracy on test set
predictions <- predict(fit, test[1:ncol(test)-1])
accuracy <- RMSE(predictions, test[["sentimentr_score"]])

# Plot results
results <- tibble(actual = test[["sentimentr_score"]],
                  predictions = predictions) %>% 
  arrange(actual)
results["paired"] <- 1:nrow(results)
results <- melt(results, id.vars = "paired")

# Data to be plotted
set.seed(500)
plot_data <- filter(results, paired %in% sample(1:nrow(test), 2500))
plot_actual <- filter(plot_data, variable == "actual")

randomForest_plot <- ggplot(plot_data, aes(x = paired, y = value)) +
  geom_point(aes(color = variable)) +
  geom_line(aes(group = paired), size = 0.3, color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_line(data = plot_actual, aes(x = paired, y = value), size = 2, alpha = 0.4) +
  ggtitle("Random forest: Prediction of reviews' sentiment score",
          subtitle = paste0("Range of sentiment score: min = ", round(min(dataset$sentimentr_score), 2),
                            " , max = ", round(max(dataset$sentimentr_score), 2), "\nRMSE = ", round(accuracy, 2))) +
  xlab("Reviews ordered by actual sentiment score") +
  ylab("Sentiment score") + 
  scale_color_manual(values = c("black", "red")) +
  scale_alpha(guide = "none")
