library(fpp3)

# mean and sd of the test set
cat("The mean of the homicide rate on the tset set is:", mean(test_set$homicide_rate))
cat("The standard deviation of the homicide rate is:", sd(test_set$homicide_rate))

# Calculate the overall average homicide rate and use it as a metric for the evaluation with the RMSE
average <- mean(train_set$homicide_rate)

# Make predictions using the average
predictions <- rep(average, length(test_set$homicide_rate))

# Calculate the RMSE
rmse <- sqrt(mean((test_set$homicide_rate - predictions)^2))

rmse

# Calculate the mean of homicide_rate by each country
b_country <- train_set %>%
  group_by(Country) %>%
  summarize(b_country = mean(homicide_rate - average))

# Predict homicide rates with the country effect
predictions <- train_set %>%
  left_join(b_country, by = "Country") %>%
  mutate(pred = average + b_country) %>%
  pull(pred)

# Evaluate the performance using RMSE 
RMSE <- RMSE(test_set$homicide_rate, predictions)

RMSE


# library for the test
library(tseries)

# Augmented Dickey-Fuller Test for stationarity
adf_result <- adf.test(train_set$homicide_rate)
cat("ADF Statistic:", adf_result$statistic)
cat("p-value:", adf_result$p.value)

# ARIMA model (Auto Regressive Integrated Moving Average)
library(forecast)

# ARIMA model
# Using homicide_rate as the series to predict
ts_data <- ts(train_set$homicide_rate, frequency = 1)

# Using auto arima to select an appropriate ARIMA model based on the AIC (Akaike Information Criterion) value
arima_model <- auto.arima(ts_data)

# inspect the model
summary(arima_model)

# Make predictions using forecast
forecast_result <- forecast(arima_model, h = 1, level = c(95))

# Plot the results
autoplot(forecast_result, main = "ARIMA Forecast", xlab = "Time", ylab = "Homicide Rate")

# Extract the last value of the training set
last_value <- tail(ts_data, 1)

# Combine the last value of the training set with the predicted mean
predicted_original <- last_value + as.numeric(forecast_result$mean)

# Create a time series for the test set
ts_test <- ts(test_set$homicide_rate, frequency = 1)

# Calculate RMSE
RMSE_value <- RMSE(predicted_original, ts_test)
print(paste("RMSE:", RMSE_value))





# ARIMAX using only GDP
# Using homicide_rate as the series to predict with one exogenous variable GDP since the correlation is the least weak
s_data <- ts(train_set[, c("homicide_rate", "GDP")], frequency = 1)

# Grid search for p, d, and q
best_model <- NULL
best_aic <- Inf

for (p in 0:3) {
  for (d in 0:1) {
    for (q in 0:3) {
      current_model <- Arima(s_data[, "homicide_rate"], order = c(p, d, q), xreg = s_data[, "GDP"], optim.control = list(maxit = 1000))
      current_aic <- AIC(current_model)
      
      if (current_aic < best_aic) {
        best_model <- current_model
        best_aic <- current_aic
      }
    }
  }
}

# Print the best model and its AIC
print(best_model)
cat("Best AIC:", best_aic)

# Summary of the best model
summary(best_model)

# Use the forecast() function with the exogenous variables for making predictions
forecast_result <- forecast(best_model, h = 1, xreg = tail(s_data[, "GDP"], 1), level = c(95))

# Combine the last value of the training set with the predicted differences
predicted_diff <- forecast_result$mean
predicted_original <- tail(train_set$homicide_rate, 1) + as.numeric(cumsum(predicted_diff))

# Calculate RMSE
RMSE <- RMSE(predicted_original - test_set$homicide_rate)
cat("RMSE:", RMSE)

# Visualize the forecast and the confidence intervals
plot(forecast_result, main = "ARIMAX Forecast", xlab = "Time", ylab = "Homicide Rate")
lines(train_set$homicide_rate, col = "blue", lty = 1, lwd = 2)  # observed values



# ARIMAX model using all 3 variables
# Using homicide_rate as the series to predict using unemployment gdp and gdp_pc
ts_data <- ts(train_set[, c("homicide_rate", "unemployment_rate", "GDP_pc", "GDP")], frequency = 1)

# Grid search for p, d, and q
best_model <- NULL
best_aic <- Inf

for (p in 0:3) {
  for (d in 0:1) {
    for (q in 0:3) {
      current_model <- Arima(ts_data[, "homicide_rate"], order = c(p, d, q), xreg = ts_data[, c("unemployment_rate", "GDP_pc", "GDP")])
      current_aic <- AIC(current_model)
      
      if (current_aic < best_aic) {
        best_model <- current_model
        best_aic <- current_aic
      }
    }
  }
}

# Print the best model and its AIC
print(best_model)
cat("Best AIC:", best_aic)

# Summary of the best model
summary(best_model)

# Use the forecast() function with the exogenous variables for making predictions
forecast_result <- forecast(best_model, h = 1, xreg = tail(ts_data[, c("unemployment_rate", "GDP_pc", "GDP")], 1), level = c(95))

# Combine the last value of the training set with the predicted mean
predicted_original <- tail(train_set$homicide_rate, 1) + as.numeric(forecast_result$mean)

# Calculate RMSE
RMSE <- RMSE(predicted_original - test_set$homicide_rate)
cat("RMSE:", RMSE)

# Visualize the forecast and the confidence intervals
plot(forecast_result, main = "ARIMAX Forecast", xlab = "Time", ylab = "Homicide Rate")
lines(train_set$homicide_rate, col = "blue", lty = 1, lwd = 2)  # observed values




# ARMA model using auto.arima()
# Using homicide_rate as the series to predict
ts_data <- ts(train_set[, "homicide_rate"], frequency = 1)

# Using auto.arima to automatically select the best ARMA model
arma_model <- auto.arima(ts_data)

# Print the best ARMA model and its AIC
print(arma_model)
cat("AIC for ARMA:", AIC(arma_model))

# Summary of the best ARMA model
summary(arma_model)

# Use the forecast() function for making predictions
forecast_result_arma <- forecast(arma_model, h = 1, level = c(95))

# Combine the last value of the training set with the forecast result
predicted_original_arma <- tail(train_set$homicide_rate, 1) + as.numeric(forecast_result_arma$mean)

# Calculate Mean Absolute Error (MAE)
RMSE_arma <- RMSE(predicted_original_arma - test_set$homicide_rate)
cat("RMSE for ARMA:", RMSE_arma)

# Visualize the forecast and the confidence intervals for ARMA
plot(forecast_result_arma, main = "ARMA Forecast", xlab = "Time", ylab = "Homicide Rate")
lines(train_set$homicide_rate, col = "blue", lty = 1, lwd = 2)  # observed values