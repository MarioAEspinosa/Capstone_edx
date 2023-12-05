# perform PCA on x_scaled
pca <- prcomp(x_scaled)

# get the summary of the PCA results
summary(pca)

# plot the scree plot to visualize the proportion of variance explained by each PC
plot(pca, type = "l", main = "Scree Plot")

# plot the first two principal components
ggplot(data.frame(pca$x), aes(x = PC1, y = PC2, color = brca$y)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", title = "PCA Plot")

# calculate the cumulative proportion of variance explained
cumulative_prop_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))

# find the index of the first principal component that explains at least 90% of the variance
n_pc <- which(cumulative_prop_var >= 0.9)[1]

# extract the first 10 principal components
pca_scores <- pca$x[, 1:10]

# create a data frame with the first 10 PCs and tumor type as a factor
df <- data.frame(pca_scores, tumor_type = factor(brca$y))

# reshape the data frame so that each row corresponds to a single PC score
df_long <- df %>% 
  pivot_longer(cols = 1:10, names_to = "PC", values_to = "score")

# create a boxplot of the first 10 PCs grouped by tumor type
ggplot(df_long, aes(x = PC, y = score, fill = tumor_type)) +
  geom_boxplot() +
  scale_x_discrete(labels = paste0("PC", 1:10)) +
  labs(x = "Principal Component", y = "Score", title = "Boxplot of the First 10 PCs Grouped by Tumor Type")
