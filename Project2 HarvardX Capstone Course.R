#An Introduction to Topic Modeling and Sentiment Analysis: 
#Amazon Food Reviews Data
#This analysis is based on product reviews from Amazon. The original dataset is 
#quite large with the following statistics:
#Number of reviews	568,454
#Number of users	256,059
#Number of products	74,258
#I ran my analysis on the whole dataset, but R was very slow, so I decided to
#sample by productid and use the smaller dataset to complete this project.  I
# also brought in a subset of the original dataset to make R coding more 
#efficient. 
#The purpose of this analysis is to do text analytics and topic modeling on a 
#sample of Amazon product reviews, mainly food reviews.  I also conducted 
#sentiment analysis.

#Install and load packages into R
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse) # metapackage of all tidyverse packages
library(lubridate)
library(lda)
library(topicmodels)
library(tm)
library(wordcloud)
library(widyr)
library(Matrix)
library(kableExtra)
library(textdata)
library(syuzhet)
library(readxl)
library(latexpdf)
library(tinytex)
theme_set(theme_light())

#load dataset into R and eliminate null reviews, assign lower cases to variables
reviews <- read_excel("/Users/alevcieslinski/Downloads/reviews3.xls")
reviews <- na.omit(reviews) 
reviews <- reviews %>%
  rename_with(str_to_lower)

#Check the structure of data
str(reviews)

#data cleanup
reviews$text <- gsub("<[^>]+>", "", reviews$text)

#summary data. I brought in productid, text(review), and score that customers
#assigned products.

reviews %>% summarize(unique_products = length(unique(productid)),
                      unique_text = length(unique(text)),
                      unique_score = length(unique(score)))

#count by score (customer review score)
reviews %>% 
  count(score, sort=TRUE)

#visual representation of score distribution
reviews %>%
  ggplot(aes(score)) +
  geom_histogram(fill="cyan", binwidth=1) +
  scale_y_continuous(labels = scales::comma) 

#tokenization, stopwords
#count words by score and productid
#In order to do the text analysis and topic modeling, we have to create words 
#from each line of text.
food_reviews <- reviews %>%
  unnest_tokens(input=text, output=word) %>%
  anti_join(stop_words, by='word') %>%
  filter(str_detect(word, "[a-z]")) %>%
  count(productid, word, score, sort=TRUE) %>%
  ungroup()

total_words <- food_reviews %>%
  group_by(productid) %>%
  summarize(total = sum(n))

food_reviews <- left_join(food_reviews, total_words)

#tf_idf
#The statistic tf-idf is intended to measure how important a word is to 
#a document in a collection (or corpus) of documents. In this case, we're 
#looking at words by productid.

food_reviews <- food_reviews %>%
  bind_tf_idf(word, productid, n) %>%
  arrange(desc(tf_idf))
glimpse(food_reviews)

#The LDA() function operates on a Document Term Matrix (DTM), so we need to 
#create a DTM from the reviews. The dataframe food_reviews containing all of 
#the word counts by productid will then be cast into a document term matrix with
#the following code.

dtm <- food_reviews %>%
  cast_dtm(document=productid, term=word, value=n)
dtm

#When we do topic modeling, we need to set some parameters, i.e.,the number of 
#topics to find in the corpus. For simplicity, we chose 5 topics.  LDA is a
#clustering algorithm.

lda <- LDA(dtm, k = 5, control = list(seed = 1968))
lda

#We can look at which words occur most commonly within each topic. Beta is 
#equal to P(word|topic), the conditional probablity that the word is observed,
#given the topic.  The code below look at the top 10 words for each of the five 
#topics modeled.  Even though the reviews are for food items, there are some
#records with other product reviews. Interestingly, topic modeling was able to 
#detect reviews about movies and identify those texts as a distinct set of topic 
#and words as shown by the first topic in the plot below. 

review_topics <- tidy(lda, matrix = "beta")
review_top_terms <- review_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

review_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#The product id for the movie reviews is B00004CI84 and we can see the original
#texts for this productid below for further validation:

reviews %>% filter(reviews$productid == "B00004CI84")

#summarize betas below
summary(review_topics)

#While beta refers to the conditional probability of a word given a topic,
#gamma is the per-document likelihood of each topic. In the code below, 
#for example, we see how the algorithm allocates topics across the first 
#two productids.

review_documents <- tidy(lda, matrix = "gamma")
review_documents <- arrange(review_documents, document)
review_documents

# Compute perplexity score
#Perplexity score is a measure of how well the model predicts a sample.  The 
#lower the score, the better the model is deemed to be.  Of course, 1 perplexity
#score is not that meaningful as we need a distribution of benchmarks to 
#be able to determine the lowest score.

perplexity(object=lda, newdata=dtm)

#Finding the optimal number of k scores
#create a dataframe to store the perplexity scores for different values of k
#As sated above, we create a set of perplexity scores based on different values 
#of k to determine the best model.
p = data.frame(k = c(2,4,8,16,32,64,128), perplexity = NA)

# loop over the values of k in data.frame p 
for (i in 1:nrow(p)) {
  print(p$k[i])
  #calculate perplexity for the given value of k
  m = LDA(dtm, method = "Gibbs", k = p$k[i],  control = list(alpha = 0.01))
  # store result in our data.frame
  p$perplexity[i] = perplexity(m, dtm)
}

#plot perplexity & values of k
ggplot(p, aes(x=k, y=perplexity)) + geom_line()

#we see that the perplexity score sharply declines for values of k greater
#than 25. 

#wordcloud
#Next, we can create a wordcloud to see a visual depiction of relative 
#importance of words by generating the counts of words in the corpus
word_frequencies <- food_reviews %>% 
  count(word)

#Create the wordcloud
wordcloud(words=word_frequencies$word, 
          freq=word_frequencies$n,
          min.freq=5,
          max.words=50,
          colors=c("DarkOrange","Blue"),
          scale=c(3,0.3))

#top 50 words that were used on product reviews
food_reviews %>% 
  count(word, sort = TRUE) %>% 
  head(50) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  labs(title="Top 50 words used in Reviews") +
  coord_flip()

#While the wordcloud and the top 50 words graph is helpful in understanding
#what seems to be most used "word/s", it doesn't help us understand 
#sentiments and emotions, which is what we will explore next.

#Sentiment analysis
#Most words by themselves do not describe sentiment. I used the sentiment
#function below to assign sentiments to the text from the "reviews" dataset.
#Once we run the code and look at s created by the get nrc sentiment , we see 
#that each row represents a review and each column represents the different 
#sentiments along with positive and negative score for that review. We also
#created a final review score that is represented below:

sentiment_data_all <- iconv(reviews$text)
s <- get_nrc_sentiment(sentiment_data_all)
s$score <- s$positive - s$negative
head(sentiment_data_all)
s[1:10,]

#check overall sentiment and represent it visually with a bar plot.
review_score <- colSums(s[,])
print(review_score)

#Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')

#sentiment score by productid
#Next, we can look at the sentiment score by product id and also see a visual
#representation of how top words contribute to "negative" and "positive" 
#emotions.
sentiment_data <- food_reviews %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(productid, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) 

head(sentiment_data)%>%
  kable() %>%
  kable_styling(bootstrap_options = "basic", full_width = F)

sentiment_analysis_word_count <- food_reviews %>% 
  inner_join(get_sentiments("bing"), "word") %>% 
  count(word, sentiment, score, sort = TRUE) %>% 
  ungroup()

sentiment_analysis_word_count %>% 
  group_by(sentiment) %>% 
  top_n(12, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to Sentiment", x = NULL) + 
  coord_flip()

#Finally, I looked at the avg. score (as provided by the initial review dataset) 
#to see whether there was a significant difference between
#"positive" and "negative" sentiments. As expected, the "negative" category 
#was lower than the "positive" category, but by a small margin.

#Avg. customer score by sentiment
sentiment_analysis_word_count %>%
  group_by(sentiment) %>%
  summarise_at(vars(score), list(name = mean))

 
