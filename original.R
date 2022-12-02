#loading packages
library(ggraph)
library(ggplot2)
library(igraph)
library(ldatuning)
library(lubridate)
library(rvest)
library(stringr)
library(text2vec)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
library(widyr)

#scrape webpages for text
#url object
main_url <- read_html('https://www.lesswrong.com/s/XsMTxdQ6fprAQMoKi')

#getting the links
hrefs <- html_nodes(main_url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "PostsItem2-title", " " ))]') %>%
  html_children() %>%
  html_children() %>%
  html_children() %>%
  html_attr('href')

#removing NAs
hrefs <- na.omit(hrefs)

#pasting hrefs to root url to create links
#empty vector for loop
links <- vector()

#looping for each href
for (i in seq_along(hrefs)) {
  links[i] <- paste0('https://www.lesswrong.com', hrefs[i])
}
#View(links)

#cleaning up work space
remove(hrefs, i)

#text and date objects
text <- vector()
date <- vector()

#for loop to get text, date from pages
for (i in seq_along(links)) {
  new_url <- read_html(links[i])

  text_data <- html_nodes(new_url, xpath = '//p') %>%
    html_text()

  text_data <- paste(text_data, collapse = ' ')

  date_data <- html_nodes(new_url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "PostsPageDate-date", " " ))]//span') %>%
    html_text()

  text <- c(text, text_data)
  date <- c(date, date_data)
}

#combine objects into tibble
data <- tibble('url' = links, 'date' = date, 'text' = text)

#cleaning up work space
remove(main_url, new_url, date, date_data, i, links, text, text_data)

#convert date format
data$date <- dmy(data$date)

#one-word-per-line format
text <- data %>%
  unnest_tokens(word, text)

#top occurring words
text %>%
  count(word, sort = TRUE)

#remove stop words
data("stop_words")
text <- text %>%
  anti_join(stop_words)

#top occurring words again
text %>%
  count(word, sort = TRUE)

###################
###################

#visual of top occurring words
text %>%
  count(word, sort = TRUE) %>%
  top_n(n = 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

##################
##################

#sentiment analysis
#negative sentiments object
bing_negative <- get_sentiments('bing') %>%
  filter(sentiment == 'negative')

#top negative sentiments
negatives <- text %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

#visual of top negative tokens
negatives %>%
  filter(n >= 10) %>%
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
  filter(n >= 8) %>%
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

#term-frequency matrix
words_by_url <- data %>%
  unnest_tokens(word, text) %>%
  count(url, word, sort = TRUE)

#determine token count per url
url_words <- words_by_url %>%
  group_by(url) %>%
  summarize(total = sum(n))

#combine words_by_url and url_words objects
words_by_url <- left_join(words_by_url, url_words)
View(words_by_url)

#binding tf and idf to tidy data
words_by_url <- words_by_url %>%
  bind_tf_idf(word, url, n)

#remove total and sort descending order
words_by_url <- words_by_url %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tokens <- data %>%
  unnest_tokens(word, text)

#compare top words by urls with all tokens
unique(words_by_url$word[1:25])
tokens$word[1:25]

#bigram analysis
bigrams_by_url <- data %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2)

#count and sort bigrams
bigrams_by_url %>%
  count(bigram, sort = TRUE)

#separate bigrams into two columns
bigrams_separate <- bigrams_by_url %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

#binding tf and idf to tidy data, then sorting
bigrams_tfidf <- bigrams_by_url %>%
  count(url, bigram) %>%
  bind_tf_idf(bigram, url, n) %>%
  arrange(desc(tf_idf))

#compare tokens, words by url, and bigrams
tokens$word[1:25]
unique(words_by_url$word[1:25])
bigrams_tfidf$bigram[1:25]

#visual of bigram relationships
bigrams_count <- function(data) {
  data %>%
    unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
    separate(bigram, c('word1', 'word2'), sep = ' ') %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

bigrams_visual <- function(bigrams) {
  set.seed(1234)
  x <- grid::arrow(type = 'closed', length = unit(0.15, 'inches'))

  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = x) +
    geom_node_point(color = 'red', size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

visual <- data %>%
  bigrams_count()

visual %>%
  filter(n > 5) %>%
  bigrams_visual()

#word correlations
#new data object of tokens
data_two <- data %>%
  unnest_tokens(word, text)

#pairwise correlations
word_correlations <- data_two %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, url, sort = TRUE) %>%
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
  filter(correlation > 0.05) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = 'red', size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#cleaning up work space
remove(bigrams_by_url, bigrams_separate, bigrams_tfidf, data_two, tokens,
       url_words, visual, word_correlations, words_by_url, bigrams_count,
       bigrams_visual)

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

#testing word contexts
sims <- sim2(x = vectors, y = vectors['smarter', , drop = FALSE],
             method = 'cosine', norm = 'l2')
head(sort(sims[,1], decreasing = TRUE), 5)

context_test <- vectors['smarter', , drop = FALSE] -
  vectors['arguments', , drop = FALSE] +
  vectors['economy', , drop = FALSE]

sims <- sim2(x = vectors, y = context_test,
             method = 'cosine', norm = 'l2')
head(sort(sims[,1], decreasing = TRUE), 5)
