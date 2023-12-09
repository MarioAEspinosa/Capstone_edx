#############
library(dplyr)
library(readxl)
library(tibble)
library(tidyr)
library(jsonlite)
library(purrr)
library(data.table)
library(caret)
library(ggplot2)
library(reshape2)
library(mice)
library(fpp3)
#############

# International Monetary Fund data set Real GDP growth (Annual percent change)
# https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD #
imf_dirty <- read_xls("imf-dm-export-20231116.xls")

# Display the first few rows of the imf data
head(imf_dirty)

# Check the class of imf
class(imf_dirty)

# Make a new data set for the preservation of the original
imf <- imf_dirty

# Renaming the first column to "Country"
colnames(imf)[1] <- "Country"

# Convert the columns containing years to numeric
imf[, -1] <- apply(imf[, -1], 2, as.numeric)

# Delete the row that is empty 
imf <- imf[-1, ]

# As both of the tables are different, I will use gather to reshape imf 
imf_reshaped <- gather(imf, key = "Year", value = "GDP", -Country)

# Let's see how the data set looks now 
head(imf_reshaped)

# Before merging the data, let's be sure that the year is numeric and filter the years 
imf_reshaped$Year <- as.numeric(imf_reshaped$Year)

imf_reshaped <- imf_reshaped %>% filter(Year >= 2000 & Year <= 2020)

summary(imf_reshaped)

# International Monetary Fund data set GDP per capita, current prices(Purchasing power parity; international dollars per capita)
gdp_pc_dirty <- read_xls("imf-dm-export-20231127.xls")

# Display the first few rows of the gdp_pc data
head(gdp_pc_dirty)

# Check the class of gdp_pc
class(gdp_pc_dirty)

# Renaming the first column to "Country"
colnames(gdp_pc_dirty)[1] <- "Country"

# Convert the columns containing years to numeric
gdp_pc_dirty[, -1] <- apply(gdp_pc_dirty[, -1], 2, as.numeric)

# Delete the row that is empty 
gdp_pc_dirty <- gdp_pc_dirty[-1, ]

# As both of the tables are different, I will use gather to reshape gdp_pc
gdp_pc_reshaped <- gather(gdp_pc_dirty, key = "Year", value = "GDP_pc", -Country)

head(gdp_pc_reshaped)

# Before merging the data, let's be sure that the year is numeric and filter the years 
gdp_pc_reshaped$Year <- as.numeric(gdp_pc_reshaped$Year)

gdp_pc_reshaped <- gdp_pc_reshaped %>% filter(Year >= 2000 & Year <= 2020)

summary(gdp_pc_reshaped)

# Since these two data sets are similar, let's join them
gdp_merged <- left_join(gdp_pc_reshaped, imf_reshaped, by = c("Country", "Year"))

head(gdp_merged)

# Victims of intentional murder data from United Nations Office of Drugs and Crime 
# https://dataunodc.un.org/dp-intentional-homicide-victims # 
homicide_dirty <- read_excel("data_cts_intentional_homicide.xlsx", skip = 1)

# Display the first few rows of the homicide data
head(homicide_dirty)

# Summary of the homicide set
summary(homicide_dirty)

# Check the class of homicide
class(homicide_dirty)

# Check the column names of homicide
names(homicide_dirty)

# Making a new data set for the cleaning and preservation of the original
homicide <- homicide_dirty

# Set the column names based on the first row
colnames(homicide) <- as.character(homicide[1,])

# Remove the first row (it's now used as column names)
homicide <- homicide[-1, ]

# Let's start by filtering only the relevant years 
homicide <- homicide %>% filter(Year >= 2000 & Year <= 2020)

# Check unique values in the "Source" column
unique(homicide$Source)

# Remove the "Source" column
homicide <- subset(homicide, select = -Source)

# Check unique values in the "Unit" column
unique(homicide$`Unit of measurement`)

# I am only going to use the Rate per 100 k population 
homicide <- homicide %>% filter(`Unit of measurement` == "Rate per 100,000 population")

# The unit of measurement is the same for all countries
homicide <- subset(homicide, select = -`Unit of measurement`)

# Rename the column value to homicide_rate
colnames(homicide)[colnames(homicide) == "VALUE"] <- "homicide_rate"

# Check the column Age
unique(homicide$Age)

# And the column Sex 
unique((homicide$Sex))

# Both columns may be good to use for future analysis, but the criminal rate as total is going to be 
homicide <- homicide %>% filter(Sex == "Total")
homicide <- homicide %>% filter(Age == "Total")

# Check the column Dimension
unique(homicide$Dimension)

# Filter the data to keep only rows where Dimension is "Total" and remove the "Dimension" column
homicide <- homicide %>%
  filter(Dimension == "Total") %>%
  select(-Dimension)

# The Iso3_code will be important to merge the data 
colnames(homicide)[colnames(homicide) == "Iso3_code"] <- "iso_code"

# Check the category column
unique(homicide$Category)

# Let's keep only the total
homicide <- filter(homicide, Category == "Total")

# Remove the Category column
homicide <- subset(homicide, select = -Category)

# Check the column indicator
unique(homicide$Indicator)

# I am only interested in the victims of intentional homicide 
homicide <- filter(homicide, Indicator == "Victims of intentional homicide")

# Remove the column indicator 
homicide <- subset(homicide, select = -Indicator)

# Check how the data set looks now
head(homicide)

# Now that we have the Country, Region, and subregion, indicator, sex, and year we could merge the two data sets 

# Unemployment, total (% of total labor force) (modeled ILO estimate) by The World Bank 
# https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS?most_recent_year_desc=false #
unemployment_dirty <- read.csv("API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_5994651.csv", skip = 4)

# Display the first row of the data set
head(unemployment_dirty)
length(unique(unemployment_dirty$Country.Name))
# Summary
summary(unemployment_dirty)

# Check the class of the data set 
class(unemployment_dirty)

# Check the column names 
names(unemployment_dirty)
head(unemployment_dirty)
# Display the data types of the first row 
sapply(unemployment_dirty[1, ], class)

# Make another data set for preservation of the original
unemployment <- unemployment_dirty

# Apparently, we have a problem with the years; some are logical, and others are numeric

# Identify the columns to convert to numeric
numeric_columns <- names(unemployment)[grepl("^X\\d{4}$", names(unemployment))]

# Convert identified columns to numeric
unemployment[, numeric_columns] <- apply(unemployment[, numeric_columns], 2, as.numeric)

# Rename the country code 
colnames(unemployment)[colnames(unemployment) == "Country.Code"] <- "iso_code"

# Display the indicator name 
unique(unemployment$Indicator.Name)

# Since there is no need for this, we will remove it 
unemployment <- subset(unemployment, select = - Indicator.Name)

# Check the indicator code 
unique(unemployment$Indicator.Code)

# Since there is no need, let´s also remove it 
unemployment <- subset(unemployment, select = - Indicator.Code)

# Rename Country.Name to Country for future use 
colnames(unemployment)[colnames(unemployment) == "Country.Name"] <- "Country"

# Reshape the data to fit the other data sets
unemployment_reshaped <- gather(unemployment, key = "Year", value = "unemployment_rate", -Country, -iso_code)

# Extract the numeric part from the 'Year' column
unemployment_reshaped$Year <- sub("X", "", unemployment_reshaped$Year)

head(unemployment_reshaped)

# And before joining the data, let's filter the Years
unemployment_reshaped <- unemployment_reshaped %>% filter(Year >= 2000 & Year <= 2020)

### Join the data sets

# Convert Year column to numeric in all data frames
homicide$Year <- as.numeric(homicide$Year)
imf_reshaped$Year <- as.numeric(imf_reshaped$Year)
unemployment_reshaped$Year <- as.numeric(unemployment_reshaped$Year)

# Before joining the data sets, we need to ensure that the names are correct 

# Inspect the country names to find discrepancies 
unique(gdp_merged$Country)
unique(unemployment_reshaped$Country)
unique(homicide$Country)

## The same country with different names in each set "Venezuela, RB"  "Venezuela (Bolivarian Republic of)" "Venezuela" ##
# It's not important on the sets with iso_code, but in the gdp, we don't have it 

# Merge datasets based on iso_code and years (since unemployment and homicide can be merged with no issues)
homicide_unemployment <- left_join(homicide, unemployment_reshaped, by = c("iso_code", "Year"))

# Inspect the new merged data
head(homicide_unemployment)

# Remove the redundant country.y and rename the country x
homicide_unemployment <- select(homicide_unemployment, -Country.y)
colnames(homicide_unemployment)[colnames(homicide_unemployment) == "Country.x"] <- "Country"

# Now moving to the gdp_merged set, the json data for the iso_code is found in this link 
## Link 
url <- "https://www.imf.org/external/datamapper/api/v1/countries"

# Fetch JSON data from the URL
json_data <- fromJSON(url) 


# Extract abbreviations and labels
json_country <- map_df(json_data$countries, ~ tibble(abbreviation = names(.x), Country = .x$label))
json_iso <- map_df(json_data, ~ tibble(
  abbreviation = names(.x),
  Country = .x$label
))

# A bit of a problem with the length (the last two in json_iso)
tail(json_country)
tail(json_iso)

json_iso <- json_iso %>%
  filter(!abbreviation %in% c("version", "output-method"))

# Merge with country names from JSON data
json_merged <- bind_cols(json_iso, json_country)

# Rename the columns and eliminate the second column
json_merged <- json_merged %>%
  select(iso_code = abbreviation...1, Country)

# Now let's join the json with the gdp_merged 
gdp_iso <- left_join(gdp_merged, json_merged, by = "Country", multiple = "all")

head(gdp_iso)
head(homicide_unemployment)

# To make sure that only one combination of iso_code and Year goes into the merging 
gdp_iso <- gdp_iso %>%
  distinct(iso_code, Year, .keep_all = TRUE)

# Join the data sets
final_merged_dataset <- left_join(homicide_unemployment, gdp_iso, by = c("iso_code", "Year"))

length(gdp_iso)
length(homicide_unemployment)
length(final_merged_dataset)

# Examine the resulting table
head(final_merged_dataset)

summary(final_merged_dataset)

# Eliminate the duplicates and rename the original ones, and there is no real need for the iso_code
final_merged_dataset <- final_merged_dataset %>%
  select(
    Country = Country.x,
    Region,
    Subregion,
    Year,
    homicide_rate,
    unemployment_rate,
    GDP_pc,
    GDP
  )

# Summary of the data frame
summary(final_merged_dataset)

# Create the validation set to keep the integrity of the original data 
# Set a seed for reproducibility
set.seed(123)

# Convert 'Year' to Date format
final_merged_dataset$Year <- as.Date(as.character(final_merged_dataset$Year), format = "%Y")

# Sort the data by Year 
final_merged_dataset <- final_merged_dataset[order(final_merged_dataset$Year), ]

# change the values to numeric
final_merged_dataset <- final_merged_dataset %>%
  mutate(homicide_rate = as.numeric(homicide_rate),
         unemployment_rate = as.numeric(unemployment_rate),
         GDP_pc = as.numeric(GDP_pc),
         GDP = as.numeric(GDP))

# Set a cutoff index for training/test split
cutoff <- floor(0.8 * nrow(final_merged_dataset))

# Split the data
train_set <- final_merged_dataset[1:cutoff, ]
test_set <- final_merged_dataset[(cutoff + 1):nrow(final_merged_dataset), ]


####################################### Missing values

# checking the countries with more missing values 
missing_values <- train_set %>%
  group_by(Country) %>%
  summarise_all(~sum(is.na(.)))

top_missing <- missing_values %>%
  gather(key = "Variable", value = "MissingCount") %>%
  arrange(desc(MissingCount)) %>%
  head(15)

top_missing

# I don´t have info about the gdp_pc an gdp in any year United Kingdom (Scotland) (Northern Ireland) and (England and wales) 
# Before eliminating them by name we could try to the interpolation method to fill the gaps 

# Detect outliers in the data set, as we are going to use the means of the variables 

# Box plot for numeric columns
numeric_columns <- c("homicide_rate", "unemployment_rate", "GDP_pc", "GDP")
par(mfrow=c(2,2))  # Set up a 2x2 grid of plots

for (col in numeric_columns) {
  boxplot(train_set[[col]], main=col, col="skyblue", border="black", notch=TRUE)
}

# Scatter plots for numeric columns
pairs(train_set[, numeric_columns], pch=16, col="darkblue")

# Define conditions for outliers
outlier_conditions <- 
  train_set$homicide_rate < quantile(train_set$homicide_rate, 0.05, na.rm = TRUE) |
  train_set$homicide_rate > quantile(train_set$homicide_rate, 0.95, na.rm = TRUE) |
  train_set$unemployment_rate < quantile(train_set$unemployment_rate, 0.05, na.rm = TRUE) |
  train_set$unemployment_rate > quantile(train_set$unemployment_rate, 0.95, na.rm = TRUE) |
  train_set$GDP < quantile(train_set$GDP, 0.05, na.rm = TRUE) |
  train_set$GDP > quantile(train_set$GDP, 0.95, na.rm = TRUE)

# Replace outlier values with NA
train_set <- train_set %>%
  mutate(
    homicide_rate = ifelse(outlier_conditions, NA, homicide_rate),
    unemployment_rate = ifelse(outlier_conditions, NA, unemployment_rate),
    GDP = ifelse(outlier_conditions, NA, GDP)
  )

# Sort the data by Year 
train_set <- train_set[order(train_set$Year), ]

# Create an imputation model
imputation_model <- mice(train_set[, c("homicide_rate", "unemployment_rate", "GDP_pc", "GDP")], method = "pmm")

# Impute missing values
imputed_data <- complete(imputation_model)

# Replace missing values in train_set with imputed values
train_set$homicide_rate[is.na(train_set$homicide_rate)] <- imputed_data$homicide_rate[is.na(train_set$homicide_rate)]
train_set$unemployment_rate[is.na(train_set$unemployment_rate)] <- imputed_data$unemployment_rate[is.na(train_set$unemployment_rate)]
train_set$GDP_pc[is.na(train_set$GDP_pc)] <- imputed_data$GDP_pc[is.na(train_set$GDP_pc)]
train_set$GDP[is.na(train_set$GDP)] <- imputed_data$GDP[is.na(train_set$GDP)]

# there is no need for the means any more
train_set <- train_set %>%
  select(Country, Region, Subregion,
         Year, homicide_rate,
         unemployment_rate, GDP_pc, GDP)

#summary 
summary(train_set)

########### Analysis correlation #####################

# Make a correlation test 

# Select relevant numeric variables
numeric_vars <- train_set %>%
  select(homicide_rate, unemployment_rate, GDP_pc, GDP) 

# Calculate correlations
cor_matrix <- cor(numeric_vars)

# Print correlation matrix
cor_matrix

# Convert the correlation matrix to a long format plot
cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("variable1", "variable2", "correlation")

# Plot the heatmap 
ggplot(cor_long, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c("blue", "white", "red"))(20)) +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  
        axis.text.y = element_text(size = 8),  
        plot.title = element_text(size = 12))  


# The correlation between them is really weak

# Mean of homicides trough the years
train_set %>% 
  group_by(Year) %>%
  summarise(mean_homicide_rate = mean(homicide_rate)) %>%
  ggplot(aes(x = Year, y = mean_homicide_rate)) +
  geom_point(color = "skyblue") +
  labs(title = "Time Series of Homicide Rate",
       x = "Year",
       y = "Homicide Mean")


# Relationship between GDP_pc and homicide rate 
train_set %>%
  ggplot(aes(x = homicide_rate, y = GDP_pc, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # regression line
  geom_text(aes(label = sprintf("Corr: %.2f", cor(homicide_rate, GDP_pc))),
            x = min(train_set$homicide_rate),
            y = max(train_set$GDP_pc),
            hjust = 0, vjust = 1, color = "black") +  # correlation coefficient
  labs(
    title = "Relationship between GDP per Capita and Homicide Rate",
    x = "Homicide Rate",
    y = "GDP per Capita"
  ) +
  theme_minimal()

# Relationship between GDP rate and homicide rate
train_set %>% 
  group_by(Country) %>%
  ggplot(aes(x = homicide_rate, y = GDP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a regression line
  geom_text(aes(label = sprintf("Corr: %.2f", cor(homicide_rate, GDP))),
            x = min(train_set$homicide_rate),
            y = max(train_set$GDP),
            hjust = 0, vjust = 1, color = "black") +  # Add correlation coefficient
  labs(
    title = "Relationship between GDP Rate and Homicide Rate",
    x = "Homicide Rate",
    y = "GDP Rate"
  ) +
  theme_minimal()

# Relationship between unemployment rate and homicide rate 
train_set %>%
  ggplot(aes(x = homicide_rate, y = unemployment_rate, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a regression line
  geom_text(aes(label = sprintf("Corr: %.2f", cor(homicide_rate, unemployment_rate))),
            x = min(train_set$homicide_rate),
            y = max(train_set$unemployment_rate),
            hjust = 0, vjust = 1, color = "black") +  # Add correlation coefficient
  labs(
    title = "Relationship between Unemployment Rate and Homicide Rate",
    x = "Homicide Rate",
    y = "Unemployment Rate"
  ) +
  theme_minimal()



######################### Models #######################

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













