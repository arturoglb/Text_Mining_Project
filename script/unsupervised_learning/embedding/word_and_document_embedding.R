source(here::here("script/setup.R"))
source(here::here("document_embedding"))
library(uwot)
library(ggrepel)
library(plotly)
library(ClusterR)
library(cluster)

# # Read data
all_reviews <- read.csv(here::here("data/smartphone_reviews_final.csv"))
apple <- read.csv(here::here("data/Apple_final.csv"))
samsung <- read.csv(here::here("data/Samsung_final.csv"))

# # Read models
all_model <- read.word2vec(here::here("script/models/word2vec/all_model.bin"))
apple_model <- read.word2vec(here::here("script/models/word2vec/apple_model.bin"))
samsung_model <- read.word2vec(here::here("script/models/word2vec/samsung_model.bin"))

# # Create interactive plot of words
# source: https://cran.r-project.org/web/packages/word2vec/readme/README.html
embedded_words <- as.matrix(all_model)
viz <- umap(embedded_words, n_neighbors = 25, n_threads = 5, n_components = 3)

# Create the dataframe used for the plot
df  <- data.frame(word = gsub("//.+", "", rownames(embedded_words)), 
                  xpos = gsub(".+//", "", rownames(embedded_words)), 
                  x = viz[, 1], y = viz[, 2], z = viz[, 3],
                  stringsAsFactors = FALSE)

# Subset the dataframe
set.seed(456)
df  <- df[sample(1:nrow(df), 1500),]

# Interactive 2D plot
plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = "text", text = ~word)

# Interactive 3D plot
plot_ly(df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = 'text', text = ~word)

# # Embed text using word embedding
# Remove punctuation
all_reviews_sentences <- gsub('[[:punct:] ]+',' ', all_reviews$Reviews)

# Split sentences into words (to lower and trimmed) - Resulting in a list
document_words <- lapply(strsplit(tolower(all_reviews_sentences), " "), trimws)

# Document embedding
embedded_documents <- lapply(X = document_words, FUN = document_embedding, embedded_words_matrix = embedded_words)

# Transform list into matrix
names(embedded_documents) <- 1:length(embedded_documents)
embedded_documents <- t(bind_rows(embedded_documents))

# Save embedded words and document
write.csv(embedded_words, here::here("script/unsupervised_learning/embedding/embedding_data/embedded_words.csv"))
write.csv(embedded_documents, here::here("script/unsupervised_learning/embedding/embedding_data/embedded_documents.csv"))

