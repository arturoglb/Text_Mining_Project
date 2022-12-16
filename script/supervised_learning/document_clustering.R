source(here::here("script/supervised_learning/word_and_document_embedding.R"))

# # Clustering of documents
all_reviews_clustering <- kmeans(embedded_documents, centers = 7, nstart = 20)
all_reviews_clustering

# Subset for better visualisation
set.seed(456)
subset_index <- sample(1:nrow(embedded_documents), 500)

# Visualise clusters
clusplot(embedded_documents[subset_index,],
         all_reviews_clustering$cluster[subset_index],
         lines = 0,
         span = TRUE,
         shade = TRUE,
         color = TRUE)

# Add clusters to all_reviews dataframe
all_reviews['cluster'] <- all_reviews_clustering$cluster

# Get most frequent word by cluster
# TODO: Plot most frequent word by cluster (horizontal bar chart)
