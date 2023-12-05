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


