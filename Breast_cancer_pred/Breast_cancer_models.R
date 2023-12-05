library(randomForest)

# set the seed to 7
set.seed(7)

# k values from 3 to 21 only odd numbers
k_values <- data.frame(k = seq(3, 21, by = 2))

# Fit KNN models for each value of k
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

knn_model <- train(train_x, train_y, method = "knn", trControl = ctrl, tuneGrid = k_values)

knn_model

# predict knn
knn_pred <- predict(knn_model, newdata = test_x)
mean(knn_pred == test_y)
knn_pred

# confussion matrix for predict knn
confusionMatrix(knn_pred, test_y)

#set the seed to 9
set.seed(9)


# random forest model 
mtry <- data.frame(mtry = c(3, 5, 7, 9))
rf_model <- train(train_x, train_y, method = "rf", trControl = ctrl, tuneGrid = mtry, importance = TRUE)

rf_model$bestTune

# predict random forest
rf_pred <- predict(rf_model, newdata = test_x)
mean(rf_pred == test_y)

# plot variable importance measures
varImp(rf_model)

# confusion matrix for predict random forest
confusionMatrix(rf_pred, test_y)









