# Install packages if missing
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages(c("quanteda", "topicmodels", "LDAvis", "ggplot2", 
                   "reshape2", "tm", "ldatuning", "readtext", "tsne", "readr"))

# Load libraries
options(stringsAsFactors = FALSE)
library(quanteda)
library(topicmodels)
library(LDAvis)
library(readtext)
library(ggplot2)
library(reshape2)
library(tm)
library(readr)
library(ldatuning)

# Load data
speeches <- read.csv("/Users/Nacho/OneDrive - Universitat Jaume I/Escritorio/Erasmus/Introduction to Digital Humanities/Project/csv/speeches.csv",
                     stringsAsFactors = FALSE, nrows = 1000)

text_data <- speeches$speechContent

# Remove empty or erroneous records
text_data <- text_data[!is.na(text_data)]
if (length(text_data) == 0) stop("Error: Files could not be loaded correctly.")

# Verify that text_data is a character vector
if (!is.vector(text_data) || !is.character(text_data)) {
  stop("Error: text_data is not a valid character vector.")
}

# Convert to quanteda corpus
text_data <- as.character(text_data)
corpus_text <- corpus(text_data)

# Text preprocessing
text_data_clean <- unname(as.list(corpus_text))
text_data_clean <- sapply(text_data_clean, paste, collapse = " ")

# Define custom stopwords
custom_stopwords <- c(stopwords("de"), "herr", "frau", "wurde", "sagen", "mehr", "ja", "wohl")  

# Tokenization and cleaning
tokens_clean <- tokens(text_data_clean, 
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(custom_stopwords, padding = FALSE)  # Stopwords in German

# Create document-term matrix (DTM)
DTM <- dfm(tokens_clean)

# Remove infrequent terms
DTM <- dfm_trim(DTM, min_termfreq = 5, min_docfreq = 2)

# Remove empty documents in the DTM
DTM <- dfm_subset(DTM, rowSums(DTM) > 0)

# Verify that the DTM is not empty
if (nfeat(DTM) == 0 || ndoc(DTM) == 0) stop("Error: The DTM matrix is empty after preprocessing.")

# Check most frequent words
topfeatures(DTM, 20)  # Displays the 20 most used words

# Automatic selection of the optimal number of topics
result <- FindTopicsNumber(
  DTM,
  topics = seq(10, 50, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 1234)
)

FindTopicsNumber_plot(result)

# Adjust the number of topics based on the graph
K <- 20

# Create LDA model
topic_model <- LDA(DTM, K, method = "Gibbs", control = list(iter = 500, seed = 1234))

# Obtain term and topic distributions
tm_result <- posterior(topic_model)
beta <- tm_result$terms
theta <- tm_result$topics

# Visualize the 10 most representative terms for each topic
top_terms <- terms(topic_model, 10)
print(top_terms)

# Save the top 5 terms per topic to label the topics
top5termsPerTopic <- terms(topic_model, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

# Interactive visualization of topics with LDAvis
topicmodels_json_ldavis <- function(fitted, dtm) {
  require(topicmodels)
  require(LDAvis)
  require(quanteda)
  
  # Extract model parameters
  phi <- posterior(fitted)$terms
  theta <- posterior(fitted)$topics
  vocab <- colnames(dtm)
  doc.length <- rowSums(as.matrix(dtm))
  term.frequency <- colSums(as.matrix(dtm))
  
  json_lda <- createJSON(phi = phi, theta = theta, vocab = vocab,
                         doc.length = doc.length, term.frequency = term.frequency)
  return(json_lda)
}

# Create JSON for visualization
json_lda <- topicmodels_json_ldavis(topic_model, DTM)

# Launch interactive visualization
serVis(json_lda, open.browser = TRUE)

