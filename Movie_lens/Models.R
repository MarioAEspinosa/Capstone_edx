# calculate the average rating
average <- mean(edx$rating)

# calculate the Root Mean Squared Error using the average
RMSE(final_holdout_test$rating, average)


#  movie effect method

# Calculate b_i for each movie
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - average))


# Predict ratings with the movie effect
predict <- final_holdout_test %>%
  left_join(b_i, by = "movieId") %>%
  mutate(pred = average + b_i) %>%
  pull(pred)

# Calculate RMSE for the movie effect
RMSE(final_holdout_test$rating, predict)

# plot movie effect
qplot(b_i, data = b_i, bins = 20)


# Movie and user effect method

# compute user bias term, b_u
b_u <- edx %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - average - b_i))

# predict new ratings with movie and user bias
predict <- final_holdout_test %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = average + b_i + b_u) %>%
  pull(pred)

# calculate RMSE for movie and user effect
RMSE(predict, final_holdout_test$rating)



# Regularized movie and user effect method


# determine best lambda from a sequence
lambdas <- seq(from=0, to=10, by=0.25)

# output RMSE of each lambda, repeat earlier steps (with regularization)
rmses <- sapply(lambdas, function(l){
  
  # calculate average rating across training data
  average <- mean(edx$rating)
  
  # compute regularized movie bias term
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - average)/(n()+l))
  
  # compute regularize user bias term
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - average)/(n()+l))
  
  # compute predictions on validation set based on these above terms
  predict <- final_holdout_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = average + b_i + b_u) %>%
    pull(pred)
  
  # output RMSE of these predictions
  return(RMSE(predict, final_holdout_test$rating))
})

# plot of RMSE vs lambdas
qplot(lambdas, rmses)

# print minimum RMSE 
min(rmses)



# Final model with regularized movie and user effects


# final linear model with the minimizing lambda
lam <- lambdas[which.min(rmses)]

# compute regularized movie bias term
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - average)/(n()+lam))

# compute regularize user bias term
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - average)/(n()+lam))

# compute predictions on final_holdout_test set based on these above terms
predictions_final_model <- final_holdout_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = average + b_i + b_u) %>%
  pull(pred)

# output RMSE of these predictions
RMSE(predictions_final_model, final_holdout_test$rating)
