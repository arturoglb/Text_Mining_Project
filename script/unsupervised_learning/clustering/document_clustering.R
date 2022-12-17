source(here::here("script/setup.R"))

# # Read embedded document matrix
embedded_documents <- select(read.csv(here::here("script/unsupervised_learning/embedding/embedding_data/embedded_documents.csv")), -X)

# # Clustering of documents
set.seed(650)
number_of_centers <- 7
all_reviews_clustering <- kmeans(embedded_documents, centers = number_of_centers, nstart = 20)
cluster_size <- tibble(cluster = seq(1:number_of_centers),
                       number_of_reviews = all_reviews_clustering$size)

# Subset for better visualisation
set.seed(456)
subset_index <- sample(1:nrow(embedded_documents), 500)

# Visualise clusters
clusplot(embedded_documents[subset_index,],
         all_reviews_clustering$cluster[subset_index],
         lines = 0,
         span = TRUE,
         shade = TRUE,
         color = TRUE,
         main = "Document clustering using document embedding")

# Add clusters to all_reviews dataframe
all_reviews['cluster'] <- all_reviews_clustering$cluster

# Get most frequent word by cluster
# Function used to remove stop words
`%notin%` <- Negate(`%in%`)

# Create dataset for plotting: Get top words per cluster
cluster_topics <- all_reviews %>% 
  mutate(review_id = seq(1:nrow(all_reviews))) %>% 
  relocate(review_id, .before = "Brand") %>% 
  unnest_tokens(output = "word",
                input = "Reviews",
                to_lower = TRUE,
                strip_punct = TRUE,
                strip_numeric = TRUE) %>% 
  filter(word %notin% stop_words$word) %>% 
  group_by(cluster, word) %>% 
  tally() %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(n >= 3) %>% 
  arrange(cluster, desc(freq)) %>% 
  top_n(15, freq) %>% 
  left_join(cluster_size, by = c("cluster" = "cluster")) %>% 
  mutate(cluster_header = paste0("Cluster ", cluster, "\nn_reviews = ", number_of_reviews, ", n_words = ", round(n/freq), "\nav_length = ", round(round(n/freq)/number_of_reviews, 2)))

# Plot top words per cluster
ggplot(cluster_topics, aes(x = freq, y = reorder_within(word, freq, cluster, sep = "_"))) +
  geom_col() +
  facet_wrap(vars(cluster_header), scale = "free_y") +
  ylab("") +
  xlab("Frequency in percentage") +
  ggtitle("Topic analysis: Top 15 words per cluster of documents",
          subtitle = "Numbers, punctuation and stop words have been removed\nOnly words appearing at least 3 times are shown in the graph")
