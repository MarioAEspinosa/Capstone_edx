library(gam)
set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

# Calculate proportion of B in training set
mean(train_y == "B")

# Calculate proportion of B in test set
mean(test_y == "B")

# fit a glm on the train set
train_glm <- train(train_x, train_y, method = "glm")

# make the predicction on the test set
glm_preds <- predict(train_glm, test_x)
glm_preds
# calculate the accuracy of the model 
mean(glm_preds == test_y)

# set the seed to 5
set.seed(5)

# Fit loess model on training data
train_loess <- train(train_x, train_y, method = "gamLoess")

# Generate predictions on the test set 
loess_preds <- predict(train_loess, test_x)
loess_preds
mean(loess_preds == test_y)
