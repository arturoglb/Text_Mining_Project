# This function takes 2 arguments: A word vector (= sentence/document) and a matrix of embedded words
# It uses each embedded word values to return the value of the word vector
# Here the input word vector should be the words in a given document
# It assumes that by averaging the vector values of the words found in the
# document it's possible to summarize the information contained in a document
# as a vector 

document_embedding <- function(words, embedded_words_matrix){
  # Keep words present in the embedded_words_matrix
  look_up_words <- words[words %in% rownames(embedded_words_matrix)]
  
  # Document embedding
  # If there is more than one word in the document, do colSums
  if (length(look_up_words) > 1){
    document_embedding <- colMeans(embedded_words_matrix[look_up_words,])
    # If length = 1, don't run colSums as it will throw an error
  } else if (length(look_up_words == 1)){
    document_embedding <- embedded_words_matrix[look_up_words,]
    # If look_up_words is empty return a vector of zeros
  } else {
    document_embedding <- rep(0, ncol(embedded_words_matrix))
  }
  
  # Return embedded document
  return(document_embedding)
}
