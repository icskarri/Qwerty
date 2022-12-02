#load libraries
library(ggraph)
library(ggplot2)
library(igraph)
library(ldatuning)
library(lubridate)
library(rvest)
library(stringr)
library(text2vec)
library(textdata)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
library(widyr)

#source data
data <- read.table("TibetanGrammar.txt", sep = '\t')

data <- data %>%
  mutate(index = 1:nrow(data)) %>%
  rename('text' = 'V1') %>%
  select(index, text)

#declare empty vector
text <- vector()

#unnest tokens
text <- data %>%
  unnest_tokens(word, text, token = "words")

#top occurring words
#text %>%
#  count(word, sort = TRUE)

#remove stop words
data("stop_words")
text <- text %>%
  anti_join(stop_words)

#top occurring words
#text %>%
#  count(word, sort = TRUE)

#visual of top occurring words
text %>%
  count(word, sort = TRUE) %>%
  top_n(n = 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#sentiment analysis
bing_negative <- get_sentiments('bing') %>%
  filter(sentiment == 'negative')

#top negative sentiments
negatives <- text %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

#visual of top negative tokens
negatives %>%
  filter(n >= 5) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#positive sentiments object
bing_positive <- get_sentiments('bing') %>%
  filter(sentiment == 'positive')

#top positive sentiments
positives <- text %>%
  inner_join(bing_positive) %>%
  count(word, sort = TRUE)

#visual of top positive tokens
positives %>%
  filter(n >= 5) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#word contributions to sentiment
word_contribution <- text %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#visual of top word contributions
word_contribution %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = 'free_y') +
  coord_flip() +
  labs(x = NULL, y = 'contribution to sentiment')

#bigram analysis
bigrams <- data %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2)

#separate into two columns
bigrams_separate <- bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

#afinn lexicon object
afinn <- get_sentiments('afinn')

#example
more_words <- bigrams_separate %>%
  filter(word1 == 'more') %>%
  inner_join(afinn, by = c(word2 = 'word')) %>%
  count(word2, value, sort = TRUE)
View(more_words)

#visual of top more bigrams
more_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(15) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = 'bigrams starting with "pretty"',
       y = 'sentiment value * contributions')

#common negative bigrams
negate_word <- c('not', 'no', 'never', 'without')

#count of negative bigrams
negate_bigrams <- bigrams_separate %>%
  filter(word1 %in% negate_word) %>%
  inner_join(afinn, by = c(word2 = 'word')) %>%
  count(word1, word2, value, sort = TRUE)

#visual of top negative bigrams
negate_bigrams %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = '__'), contribution)) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = 'free') +
  scale_x_discrete(labels = function(x) gsub('__.+$', '', x)) +
  coord_flip() +
  labs(x = 'top negative bigrams',
       y = 'sentiment value * contributions')

#cleaning up work space
remove(afinn, bigrams, bigrams_separate, bing_negative, bing_positive,
       more_words, negate_bigrams, negatives, positives, stop_words, text,
       word_contribution, negate_word)

#word frequency and n-gram correlation analysis
#cleaning text
data$text <- gsub('\r?\n|\r', ' ', data$text)
data$text <- gsub('[[:punct:]]', '', data$text)
data$text <- tolower(data$text)
data$text <- removeWords(data$text, stopwords('smart'))
data$text <- stripWhitespace(data$text)

#word correlations
#new data object of tokens
data_two <- data %>%
  unnest_tokens(word, text)

#pairwise correlations
word_correlations <- data_two %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, word, sort = TRUE) %>%
  as_tibble()

#some correlations less than 0.99
word_correlations %>%
  filter(correlation < 0.99)

#correlations with specific words
#laws
word_correlations %>%
  filter(item1 == 'laws')

#language
word_correlations %>%
  filter(item1 == 'language')

#remarks
word_correlations %>%
  filter(item1 == 'remarks')

#sentences
word_correlations %>%
  filter(item1 == 'sentences')

#visual of selected words and their correlations
#position, principal, religion, science
word_correlations %>%
  filter(item1 %in% c('laws', 'language', 'remarks', 'sentences')) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ item1, scales = 'free') +
  coord_flip()

#relationship clusters with pairwise correlation
set.seed(1234)

word_correlations %>%
  #filter(correlation > 0.05) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = 'red', size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#cleaning up work space
remove(data_two, word_correlations)

#topic modeling
text <- data$text %>%
  removePunctuation() %>%
  tolower() %>%
  removeWords(stopwords('smart')) %>%
  stripWhitespace()

#convert text vector to doc term matrix
term_matrix <- VCorpus(VectorSource(text)) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace) %>%
  DocumentTermMatrix()

#remove zero entries
remove <- NULL
for(i in 1:NROW(term_matrix)){
  count.non.zero <- sum(term_matrix[i,]!=0, na.rm=TRUE)
  remove <- c(remove, count.non.zero < 1)
}
term_matrix <- term_matrix[!remove == TRUE,]
remove(count.non.zero, i, remove, text)

#first lda topic model
LDA_one <- LDA(term_matrix, k = 2, control = list(seed = 1234))

#per topic word probabilities
topics_one <- tidy(LDA_one, matrix = 'beta')
View(topics_one)

#sort by topic
top_topics <- topics_one %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#visual of topics
top_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip() +
  scale_x_reordered()

#beta spread
beta_spread <- topics_one %>%
  mutate(topic = paste0('topic', topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#visual of greatest beta spreads
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = 'log ratio (topic 2 / topic 1)') +
  coord_flip()

#calculate number of topics
topics_count <- FindTopicsNumber(term_matrix,
                                 topics = seq(from = 2, to = 20, by = 1),
                                 metrics = c('Griffiths2004',
                                             'CaoJuan2009',
                                             'Arun2010',
                                             'Devaud2014'),
                                 method = 'Gibbs',
                                 control = list(seed = 23),
                                 mc.cores = NA,
                                 verbose = TRUE)

#visual of results
FindTopicsNumber_plot(topics_count)

#second lda topic model
LDA_two <- LDA(term_matrix, k = 14, control = list(seed = 1234))

#per topic word probabilities
topics_two <- tidy(LDA_two, matrix = 'beta')
View(topics_two)

#sort by topic
top_topics_two <- topics_two %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#visual of topics
top_topics_two %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip() +
  scale_x_reordered()

#beta spread
beta_spread_two <- topics_two %>%
  mutate(topic = paste0('topic', topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#visual of greatest beta spreads
beta_spread_two %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = 'log ratio (topic 2 / topic 1)') +
  coord_flip()

#cleaning up work space
remove(beta_spread_two, beta_spread, LDA_one, LDA_two, term_matrix,
       top_topics, top_topics_two, topics_count, topics_one, topics_two)

#word embeddings
#iterator object
tokens <- space_tokenizer(data$text)

#create vocabulary of unigrams
it <- itoken(tokens, progressbar = FALSE)
vocabulary <- create_vocabulary(it)

#reduce to words with minimum frequency of 5
vocabulary <- prune_vocabulary(vocabulary, term_count_min = 5L)
length(vocabulary$term)

#term co-occurrence matrix
#vocabulary vectorizer object
vectorizer <- vocab_vectorizer(vocabulary)

#window for context words
matrix <- create_tcm(it, vocabulary, skip_grams_window = 5L)

#fitting the model
glove <- GlobalVectors$new(rank = 50, x_max = 10)
model <- glove$fit_transform(matrix, n_iter = 10,
                             convergence_tol = 0.01,
                             n_threads = 4)

#model dimensionality
dim(model)

#sum of min and context vectors
context <- glove$components
vectors <- model + t(context)

#====================================

#testing word contexts
sims <- sim2(x = vectors, y = vectors['medium', , drop = FALSE],
             method = 'cosine', norm = 'l2')
head(sort(sims[,1], decreasing = TRUE), 5)

context_test <- vectors['medium', , drop = FALSE] -
  vectors['money', , drop = FALSE] +
  vectors['replacement', , drop = FALSE]

sims <- sim2(x = vectors, y = context_test,
             method = 'cosine', norm = 'l2')
head(sort(sims[,1], decreasing = TRUE), 5)

