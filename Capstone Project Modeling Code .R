#Modeling

#I first created the RMSE function in R and then ran the simplest version of 
#the model, which is the average value of rating in the train data set.

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
#As a second step, I researched the movie bias to improve my prediction.  
#The movie bias stemmed from the fact that there were some movies that were 
#not rated by a lot of users and their outlying values could bias the estimates.  
#I was able to see an improvement of ~12% in the RMSE value.  
#The RMSE went down from 1.06 to 0.94.

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
#As a third model, I looked at the user effect since some users rated a lot of 
#movies and some rated only a few.  Controling for the user bias, I was able to 
#get the RMSE down to 0.86,  another ~9% improvement.

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
#As a next model, I studied the Regularization Method.  Regularization is 
#used to avoid over-fitting due to the effect of large errors. This method 
#reduces the impact of the magnitude of the independent variables by adding 
#a penalty term to estimates on small sample sizes.    For example, some movies 
#were rated by only a few users. Therefore, larger estimates of b_i are 
#likely to occur. Large errors can increase RMSE and we can use the 
#regularization method to correct for this.

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
#Add movie year and genre

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



