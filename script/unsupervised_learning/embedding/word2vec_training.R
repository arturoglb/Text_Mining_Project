source(here::here("script/setup.R"))

# # Read data
all_reviews <- read.csv(here::here("data/smartphone_reviews_final.csv"))
apple <- read.csv(here::here("data/Apple_final.csv"))
samsung <- read.csv(here::here("data/Samsung_final.csv"))

# # Train model
all_model <- word2vec(tolower(all_reviews$Reviews), type = "cbow", dim = 50, iter = 30)
apple_model <- word2vec(tolower(apple$Reviews), type = "cbow", dim = 50, iter = 30)
samsung_model <- word2vec(tolower(samsung$Reviews), type = "cbow", dim = 50, iter = 30)

# # Save the model
write.word2vec(all_model, here::here("script/models/word2vec/all_model.bin"))
write.word2vec(apple_model, here::here("script/models/word2vec/apple_model.bin"))
write.word2vec(samsung_model, here::here("script/models/word2vec/samsung_model.bin"))

