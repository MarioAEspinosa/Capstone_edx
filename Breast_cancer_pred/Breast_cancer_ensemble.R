# make the ensemble for the different preds
ensemble <- cbind(glm = glm_preds == "B", loess = loess_preds == "B", rf = rf_pred == "B", knn = knn_pred == "B")

# if else predicting B if votes ">=" 5
ensemble_preds <- ifelse(rowMeans(ensemble) >= 0.5, "B", "M")

# accuracy of the ensemble method 
mean(ensemble_preds == test_y)

# table for the accuracy comparison
models <- c("Logistic regression", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(glm_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_pred == test_y),
              mean(rf_pred == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)
