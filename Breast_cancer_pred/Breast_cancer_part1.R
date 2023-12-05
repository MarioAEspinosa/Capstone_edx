options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)


head(brca)

# number of samples and predictors in the data set
dim(brca$x)
# alternative we can use 
dim(brca$x)[1]
dim(brca$x)[2]


# count the values on brca$y
counts <- table(brca$y)

# take the porportion of M in brca%y
prop_M <- counts["M"] / sum(counts)
prop_M
# alternative we could use a simpler way with
mean(brca$y == "M")


# Calculate column means
means <- colMeans(brca$x)

# Identify column with highest mean
highest_mean_col <- which.max(means)
# alternative 
which.max(colMeans(brca$x))

# calculate columns standar deviations
sds <- colSds(brca$x)

# identify the column with the lowest sd
lowest_sd <- which.min(sds)
#alternative
which.max(colSds(brca$x))

# Scale each column using sweep()
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

# the standar deviation of the first column scaled
sd(x_scaled[,1])

# the mean of the first column
median(x_scaled[,1])
