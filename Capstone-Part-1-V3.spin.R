#Movielens Data. This project will utilize the following dataset:
#https://grouplens.org/datasets/movielens/10m/.  The initial code was provided by the course
#materials and we used to do the data pull as shown below:

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

movielens <- mutate(movielens, rating_year= year(as.Date(as.POSIXct(timestamp, 
                                                            origin = "1970-01-01"))))
#create movie year

movie_year <- stringi::stri_extract(movielens$title, regex = "(\\d{4})", comments = TRUE) %>% 
  as.numeric()
movielens <- movielens %>% mutate(movie_year = movie_year) %>% select(-timestamp)

summary(movielens)

movielens %>% filter(movie_year > 2018) %>% 
  group_by(movieId, title, movie_year) %>% 
  summarize(n = n())

movielens %>% filter(movie_year < 1900) %>% 
  group_by(movieId, title, movie_year) %>% 
  summarize(n = n())

#Correct movie dates dates

movielens[movielens$movieId == "27266", "movie_year"] <- 2004
movielens[movielens$movieId == "671", "movie_year"] <- 1996
movielens[movielens$movieId == "2308", "movie_year"] <- 1973
movielens[movielens$movieId == "4159", "movie_year"] <- 2001
movielens[movielens$movieId == "5310", "movie_year"] <- 1985
movielens[movielens$movieId == "8864", "movie_year"] <- 2004
movielens[movielens$movieId == "1422", "movie_year"] <- 1997
movielens[movielens$movieId == "4311", "movie_year"] <- 1998
movielens[movielens$movieId == "5472", "movie_year"] <- 1972
movielens[movielens$movieId == "6290", "movie_year"] <- 2003
movielens[movielens$movieId == "6645", "movie_year"] <- 1971
movielens[movielens$movieId == "8198", "movie_year"] <- 1960
movielens[movielens$movieId == "8905", "movie_year"] <- 1992
movielens[movielens$movieId == "53953", "movie_year"] <- 2007

#Calculate the age of movie

movielens <-movielens %>% mutate(movie_age = 2022 - movie_year)

#Create the validation set that will be 10% of MovieLens data.

set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
train <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed, edx)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = train$rating, times = 1, p = 0.1, list = FALSE)
train_data <- train[-test_index,]
temp <- train[test_index,]

#Matching userId and movieId in both train and test sets

test_data <- temp %>%
  semi_join(train_data, by = "movieId") %>%
  semi_join(train_data, by = "userId")

# Adding back rows into train set

removed <- anti_join(temp, test_data)
train_data <- rbind(train_data, removed)

rm(test_index, temp, removed)

summary(train)
head(train)

#Data Analysis

train %>% summarize(unique_users = length(unique(userId)),
                  unique_movies = length(unique(movieId)),
                  unique_genres = length(unique(genres)))

#Analysis of Rating
#Distribution and Avg. Rating by Genre 

train %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(count)) 

#Avg.rating by genre visually
train %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating)) %>%
  ggplot(aes(reorder(genres, avg_rating), avg_rating, fill= avg_rating)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Reds") + labs(y = "Avg Rating", x = "Genre") +
  ggtitle("Average Rating by Genre")

#Analysis of ratings: compare n of ratings by level
train %>% group_by(rating) %>% summarize(count = n())

#Ratings' distribution by level visually
  train %>% 
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line() +
  ggtitle("Ratings' Distribution by level")

#Relationship between movie age and ratings  
train %>% group_by(movie_age) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(movie_age, rating)) +
    geom_point() +
    geom_smooth() +
  ggtitle("Avg. Rating by Movie Age")

#Distribution of ratings by title

  train %>% group_by(title) %>% summarize(count = n()) %>%
    ggplot(aes(count)) + geom_histogram(fill = "blue", color = "grey", bins = 100) +
    labs(x = "No. of Movie Ratings", y = "Count") +
    scale_x_continuous(trans="log10") +
  ggtitle("Number of Ratings by Title")

#Distribution of ratings by users

train %>% group_by(userId) %>% summarize(count = n()) %>%
    ggplot(aes(count)) + geom_histogram(fill = "skyblue", color = "black", bins = 100) +
    labs(x = "No. of Movie Ratings", y = "Count") +
    scale_x_log10() +
  ggtitle("Number of Ratings by User")

#By user, distribution of avg. rating

user_rating <- train %>%
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(desc(count))

summary(user_rating)

train %>% group_by(userId) %>% summarize(rating = mean(rating)) %>%
  ggplot(aes(rating,userId)) +
  geom_point() +
  geom_smooth() +
ggtitle("Avg. Rating by User")

#Modeling

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu <- mean(train_data$rating)
mu

naive_rmse <- RMSE(test_data$rating, mu)
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

#effect of movie ratings 
# calculate RMSE of movie ranking effect

mu <- mean(train_data$rating) 
movie_avgs <- train_data %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- test_data %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_data$rating)

movie_bias <- RMSE(predicted_ratings, test_data$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "movie bias added", RMSE = movie_bias))
rmse_results %>% knitr::kable()

#Looking at the estimates taking into account the movie bias, we see that they vary.

qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

#effect of users

user_avgs <- train_data %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_data %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
user_bias <- RMSE(predicted_ratings, test_data$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "movie and user bias added", RMSE = user_bias))
rmse_results %>% knitr::kable()

#Regularization method

lambdas <- seq(0,10,0.25)
rmses <- sapply(lambdas, function(a){
  mu <- mean(train_data$rating)
  
  b_i <- train_data %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + a))
  
  b_u <- train_data %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + a))
  
  predicted_ratings <- test_data %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, test_data$rating))
})

rmse_results <- bind_rows(rmse_results, tibble(method = "regularization method", RMSE = min(rmses)))
rmse_results %>% knitr::kable()

qplot(lambdas, rmses, color = I("green"))

lambdas[which.min(rmses)]

#Regularization with additional attributes

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(a){
  mu <- mean(train_data$rating)
  
  b_i <- train_data %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + a))
  
  b_u <- train_data %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + a))
  
  b_y <- train_data %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(movie_year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n() + a))
  
  b_g <- train_data %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'movie_year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n() + a))
  
  predicted_ratings <- test_data %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = 'movie_year') %>%
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu + b_i +  b_u + b_y + b_g) %>% .$pred
  
  return(RMSE(predicted_ratings, test_data$rating))
})

rmse_results <- bind_rows(rmse_results, 
tibble(method = "regularization method enhanced with year and genre", RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Compute new predictions using the optimal lambda

qplot(lambdas, rmses)  

a <- lambdas[which.min(rmses)]

#Validation results

mu <- mean(validation$rating)

b_title <- validation %>%
  group_by(movieId) %>%
  summarize(b_title = sum(rating - mu)/(n() + a))

b_user <- validation %>%
  left_join(b_title, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_user = sum(rating - b_title - mu)/(n() + a))

predicted_ratings <- validation %>%
  left_join(b_title, by = "movieId") %>%
  left_join(b_user, by = "userId") %>%
  mutate(pred = mu + b_title +  b_user) %>% .$pred

RMSE(predicted_ratings, validation$rating)



