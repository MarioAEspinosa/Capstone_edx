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

## Analysis

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


