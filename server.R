#server logic
#load libraries
library(shiny)
library(tidyverse)
library(rvest)
library(ggraph)
library(ggplot2)
library(igraph)
library(ldatuning)
library(lubridate)
library(stringr)
library(text2vec)
library(textdata)
library(tidytext)
library(tm)
library(topicmodels)
library(widyr)

#source data
data <- read.table("BritishArmy.txt", sep = '\t')

data <- data %>%
  mutate(index = 1:nrow(data)) %>%
  rename('text' = 'V1') %>%
  select(index, text)

#declare empty vector
text <- vector()

#unnest tokens
text <- data %>%
  unnest_tokens(word, text, token = "words")

#remove stop words
data("stop_words")
text <- text %>%
  anti_join(stop_words)

#sentiment analysis
bing_negative <- get_sentiments('bing') %>%
  filter(sentiment == 'negative')

#top negative sentiments
negatives <- text %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

#positive sentiments object
bing_positive <- get_sentiments('bing') %>%
  filter(sentiment == 'positive')

#top positive sentiments
positives <- text %>%
  inner_join(bing_positive) %>%
  count(word, sort = TRUE)

#word contributions to sentiment
word_contribution <- text %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

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
#View(more_words)

#common negative bigrams
negate_word <- c('not', 'no', 'never', 'without')

#count of negative bigrams
negate_bigrams <- bigrams_separate %>%
  filter(word1 %in% negate_word) %>%
  inner_join(afinn, by = c(word2 = 'word')) %>%
  count(word1, word2, value, sort = TRUE)

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
  filter(correlation > 0.85)

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


#relationship clusters with pairwise correlation
set.seed(1234)


# #cleaning up work space
# remove(data_two, word_correlations)

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
#View(topics_one)
 
#sort by topic
top_topics <- topics_one %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#beta spread
beta_spread <- topics_one %>%
  mutate(topic = paste0('topic', topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))


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

#====================================

#rendering
shinyServer(function(input, output) {
  
  output$fileUpload <- renderTable(req(input$upload))
  
  output$ggplotPositiveTokens <- renderPlot({
    
    positiveTokens <- positives %>%
      filter(n >= 5) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = n)) +
      geom_col() +
      labs(x = "Token", y = "Frequency", 
           title = "Top Positive Tokens") +
      coord_flip()

    print(positiveTokens)
    
  })
  
  
  output$ggplotNegativeTokens <- renderPlot({
    
    negativeTokens <- negatives %>%
      filter(n >= 5) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = n)) +
      geom_col() +
      labs(x = "Token", y = "Frequency",
           title = "Top Negative Tokens") +
      coord_flip()

    print(negativeTokens)
    
  })
  
  output$wordContributions <- renderPlot({
    
    word_contribution %>%
      group_by(sentiment) %>%
      top_n(15) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = 'free_y') +
      coord_flip() +
      labs(x = "Token", y = 'Contribution to Sentiment',
           title = "Token Contributions to Sentiment")
    
  })
  
  output$bigramAnalysisOne <- renderPlot({
    
    more_words %>%
      mutate(contribution = n * value) %>%
      arrange(desc(abs(contribution))) %>%
      head(15) %>%
      mutate(word2 = reorder(word2, contribution)) %>%
      ggplot(aes(word2, n * value, fill = n * value > 0)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = 'Bigrams Starting with "pretty"',
           y = 'Sentiment Value * Contributions')
    
  })
  
  output$bigramAnalysisTwo <- renderPlot({
    
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
      labs(x = 'Top Negative Bigrams',
           y = 'Sentiment Value * Contributions')
    
  })
  
  output$correlationAnalysisOne <- renderPlot({
    
    #laws, language, remarks, sentences
    word_correlations %>%
      filter(item1 %in% c('laws', 'language', 'remarks', 'sentences')) %>%
      group_by(item1) %>%
      top_n(5) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      #filter(correlation > 0.5) %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat = 'identity') +
      facet_wrap(~ item1, scales = 'free') +
      labs(x = "Second Token", y = "Correlation") +
      coord_flip()

    
  })
  
  output$correlationAnalysisTwo <- renderPlot({
  
  word_correlations %>%
    graph_from_data_frame() %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = 'red', size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
  })
  
  
  output$topicModeling <- renderPlot({
    
    # top_topics %>%
    #   mutate(term = reorder_within(term, beta, topic)) %>%
    #   ggplot(aes(term, beta, fill = factor(topic))) +
    #   geom_col(show.legend = FALSE) +
    #   facet_wrap(~ topic, scales = 'free') +
    #   labs(x = "Term", y = "Beta Spread",
    #        title = "Topic Clusters") +
    #   coord_flip() +
    #   scale_x_reordered()
    
    #visual of results
    FindTopicsNumber_plot(topics_count)

    
  })
  
  
  output$betaSpreads <- renderPlot({
    
    beta_spread %>%
      group_by(direction = log_ratio > 0) %>%
      top_n(10, abs(log_ratio)) %>%
      ungroup() %>%
      mutate(term = reorder(term, log_ratio)) %>%
      ggplot(aes(term, log_ratio, fill = log_ratio)) +
      geom_col() +
      labs(x = "Term", y = 'Log Ratio (Topic 2 / Topic 1)',
           title = "Beta Spreads by Log Ratios") +
      coord_flip()
    
  })
  
  
})