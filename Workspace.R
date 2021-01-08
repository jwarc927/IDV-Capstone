# Load Packages
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(caret)) install.packages("caret")
library(caret)
if (!require(dslabs)) install.packages("dslabs")
library(dslabs)
if (!require(rpart)) install.packages("rpart")
library(rpart)

# Load Data
data_path <- "C:\\Users\\jeffw\\R projects\\BBG\\bgg_db_1806.csv"

data_raw <- read.csv(data_path)

# Clean Data 1
# Check the raw data for NA's
sum(is.na(data_raw))

# Look for duplicated vales
apply(apply(data_raw, 2, duplicated), 2 ,sum)

# Check for duplicates in the names column 
dup_names <- which(duplicated(data_raw$names))
data_raw[dup_names,]$names
data_raw %>% filter(names %in% data_raw[dup_names,]$names) %>% select(game_id, names, year) %>% arrange(names)
# Note that the duplicated names seem to be re-released games with unique game_id's, so we will retain them


# Partition Data
set.seed(5, sample.kind = "Rounding")
test_index <- createDataPartition(data_raw$rank, times = 1, p = 0.15, list = FALSE)
test <- data_raw[test_index,] %>% select(!bgg_url) %>% select(!image_url)
train <- data_raw[-test_index,] %>% select(!bgg_url) %>% select(!image_url)


# Explore Data 1
# First we construct a correlation plot between the numeric variables to visualize any relationships
numeric_variables <- c("rank", "min_players", "max_players", "avg_time", "min_time", "max_time", "year", "avg_rating", "geek_rating",
                       "num_votes", "age", "owned", "weight")
corrplot(cor(select(data_raw, numeric_variables)))

# Compare the average rating against the "geek rating" to see how the website normalizes average rating for games
# with few votes
plot(train$avg_rating, train$geek_rating)

# Histogram of average ratings
hist(train$avg_rating)

# Histogram of geek rating
hist(train$geek_rating)

# Histogram of number of times the games were rated
hist(train$num_votes)

# Plot of "geek rating" against number of ratings
plot(train$num_votes, train$geek_rating)

# Plot of average rating against number of ratings
plot(train$num_votes, train$avg_rating)

# Plot of game complexity against year of release
train %>% filter( year >= 1950) %>% ggplot(aes(year, weight)) + geom_point()

# Plot of average rating against complexity
train %>% ggplot(aes(weight, avg_rating)) + geom_point()

# Plot of "geek rating" against complexity
train %>% ggplot(aes(weight, geek_rating)) + geom_point()


# Linear Model
# Here we build a linear model based on the apparent relationship between game complexity and favorable ratings
fit_lm <- lm(avg_rating ~ weight, data = train)
fit_lm$coef
y_hat_lm <- fit_lm$coef[1] + fit_lm$coef[2] * test$weight

# Here we examine the usefulness of the linear model
avg <- mean(train$avg_rating)
mean((avg - test$avg_rating)^2)
mean((y_hat_lm - test$avg_rating)^2)


# Clean Data 2
# Here we extract the primary game mechanic and primary category out of the raw data, which combines multiple 
# descriptors into a single field.  Also, we add a field called "top10" as a boolean variable that says whether or
# not a game is in the top 10% of all games
train <- train %>%
  separate(mechanic, into = "mech_1", sep = ",", extra = "drop") %>%
  separate(category, into = "cat_1", sep = ",", extra = "drop") %>%
  mutate(top10 = as.factor(rank < length(data_raw$rank) * 0.1))

test <- test %>%
  separate(mechanic, into = "mech_1", sep = ",", extra = "drop") %>%
  separate(category, into = "cat_1", sep = ",", extra = "drop") %>%
  mutate(top10 = rank < length(data_raw$rank) * 0.1)


# Explore Data 2
# Plot of mean ratings within game mechanic / category combinations (i.e., average rating among economic card games 
# or war board games)
train %>%
  group_by(mech_1, cat_1) %>%
  select(names, mech_1, cat_1, geek_rating) %>%
  summarize(mean_rating = mean(geek_rating), n = n()) %>%
  ggplot(aes(n, mean_rating)) + geom_point()


# Decision Tree
# Here we compare decision trees designed predict whether or not a game is in the top 10%.  First we look at the 
# more obvious correlation between game ownership and game ranking, then we see how much the predictive power can
# be improved by adding in game mechanics and category
fit_tree1 <- rpart(top10 ~ num_votes + owned,
                   data = train, control = rpart.control(cp = 0, minsplit = 5))

fit_tree2 <- rpart(top10 ~ num_votes + owned + mech_1 + cat_1 + weight,
                   data = train, control = rpart.control(cp = 0, minsplit = 5))

y_hat_tree1 <- predict(fit_tree1, test, type = "class")
y_hat_tree2 <- predict(fit_tree2, test, type = "class")


# Finally, we see how much the model improves by including category, mechanics, and complexity
mean(y_hat_tree1 == test$top10)
mean(y_hat_tree2 == test$top10)








