source(here::here("script/setup.R"))

# # Read sentiment score per review
sentimentr_documents <- read.csv(here::here("script/sentiment_analysis/sentiment_data/sentimentr_per_review.csv")) %>% 
  select(review_id, brand = Brand, model = Model, sentimentr_score = sentimentr_value)

# # Read document embedded matrix
embedded_documents <- select(read.csv(here::here("script/unsupervised_learning/embedding/embedding_data/embedded_documents.csv")), -X) %>%
  mutate(review_id = as.integer(rownames(.))) %>% 
  relocate(review_id, .before = V1)

# # Training and testing dataset
set.seed(456)

# Join sentiment and embedding matrix to form the dataset
dataset <- left_join(embedded_documents, sentimentr_documents,  by = "review_id") %>% 
  select(-review_id)
dataset$brand <- as.factor(dataset$brand)
dataset$model <- as.factor(dataset$model)

# Split the data
split <- sample.split(dataset, SplitRatio = 0.75)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")

# # Train the model: Random forest regressor
nb_trees <- 100
fit <- randomForest(data = train,
                    sentimentr_score ~ .,
                    ntree = nb_trees,
                    mtry = 25,
                    importance=TRUE)

# Save the model
save(fit, file = paste0(here::here("script/models/randomForest/rf_", nb_trees, ".RData")))
