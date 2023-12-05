# we can see the top of the data set with the function head
head(edx)

# dimmensions of the data set 
dim(edx)

# zeros as ratings
edx %>% filter(rating == 0) %>% tally()

# how many threes as ratings 
edx %>% filter(rating == 3) %>% tally()

# how many movies
length(unique(edx$movieId))

# how many users 
length(unique(edx$userId))


# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})


# number of ratings 
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# number of differents ratings 
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

# plot of movies ratings 
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
