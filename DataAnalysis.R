## Spyridon Ntokos
## MovieLens Project 
## HarvardX: PH125.9x - Capstone Project
## https://github.com/spyridon-ntokos

##################################
# Create edx set, validation set #
##################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#############################
# Exploratory Data Analysis #
#############################

## Data ingestion

# Extra libraries used in analysis and visulizations
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

## Data understanding

# Taking a peek at the data
head(edx)
head(validation)

# Basic summary statistics
summary(edx)
summary(validation)

# Number of observations
length(edx$rating)
length(validation$rating)

## Data pre-processing

# Define RMSE (Root Mean Square Error) function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}

## Modify columns to appropriate format for analysis

# Extract year of the movie and save it in a new column
edx <- edx %>% 
  mutate(year = as.numeric(str_sub(title,-5,-2)),
         title = str_sub(title, 1, -8))
names(edx)[names(edx) == "year"] <- "year_release"

validation <- validation %>% 
  mutate(year = as.numeric(str_sub(title,-5,-2)),
         title = str_sub(title, 1, -8))
names(validation)[names(validation) == "year"] <- "year_release"

# Transform 'timestamp' column to year rated
edx <- edx %>% 
  mutate(timestamp = as.POSIXct(timestamp, 
                                origin = "1970-01-01", 
                                tz = "GMT"))
edx$timestamp <- format(edx$timestamp, "%Y")
names(edx)[names(edx) == "timestamp"] <- "year_rated"
edx <- edx %>% 
  mutate(year_rated = as.numeric(year_rated))
colnames(edx)

validation <- validation %>% 
  mutate(timestamp = as.POSIXct(timestamp, 
                                origin = "1970-01-01", 
                                tz = "GMT"))
validation$timestamp <- format(validation$timestamp, "%Y")
names(validation)[names(validation) == "timestamp"] <- "year_rated"
validation <- validation %>% 
  mutate(year_rated = as.numeric(year_rated))
colnames(validation)

# Extract unique combinations of genres in edx
genre_distinct <- edx %>% 
  distinct(genres)

# Define all different genre categories as a vector
genre_categories <- genre_distinct %>%
  separate_rows(genres, sep = "\\|") %>%
  distinct(genres) %>%
  .$genres


## Data exploration & general statistics

# Check datasets for consistency
head(edx)
head(validation)

# Distinct genres in edx & general genre categories
genre_distinct
genre_categories

# Number of unique users and movies in edx
edx %>% 
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId))

# Total rating per movie category
sapply(genre_categories, function(g) {
  sum(str_detect(edx$genres, g))
})

# Rating distribution
stars <- data.frame(rating = as.factor(edx$rating))
levels(stars$rating)

stars %>% 
  ggplot(aes(rating)) +
  geom_bar() +
  ggtitle("Rating distribution") +
  xlab("Stars") +
  scale_y_continuous(name = "Number of ratings", labels = scales::comma)
  
## Data analysis strategies

# User bias visualization
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10(name = "Ratings / user") +
  ylab("Number of users") +
  ggtitle("Users' rating distribution")

# Movie bias visualization
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10(name = "Received ratings") +
  ylab("Number of movies") +
  ggtitle("Rated movies distribution")

# Genre popularity
lot <- lapply(genre_categories, function(g) {
  # create a List of Tibbles (lot) with each distinctive category
  edx %>%
    filter(str_detect(genres, g)) %>%  # choose category
    mutate(genres = g) %>%             # rename genre to specific category
    select(movieId, year_release, genres) %>%  # select columns of interest 
    group_by(year_release, genres) %>% # group data by year and category
    summarize(number = n())            # number of ratings
})

# transform lot to data frame by binding list element rows
genre_popularity <- do.call(rbind, lapply(lot, data.frame, stringsAsFactors = FALSE)) %>%
  # fill with zero for every release-year without rating in specific category
  complete(year_release = full_seq(year_release, 1), genres, fill = list(number = 0))

# Genre category vs Release year visualization
genre_popularity %>%
  filter(year_release > 1930) %>%
  # only 4 randomly picked categories are chosen for readability
  filter(genres %in% c("Comedy", "Adventure", "Drama", "Mystery")) %>%
  ggplot(aes(year_release, number, col = genres)) +
  geom_line() +
  scale_y_continuous(name = "Number of ratings", labels = scales::comma) +
  xlab("Year") +
  ggtitle("Genre Popularity")

# Stars vs Release-year
edx %>% 
  group_by(year_release) %>%
  summarize(stars = mean(rating)) %>%
  ggplot(aes(year_release, stars)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Rating vs Release year") +
  xlab("Year") +
  ylab("Stars")

# Age of movie vs Rating
edx %>%
  mutate(age = year_rated - year_release) %>% # define age as the age of movie when rated
  group_by(age) %>%
  summarize(number = n()) %>%
  ggplot(aes(age, number)) +
  geom_line() +
  ggtitle("Number of ratings based on oldness") +
  xlab("Age") +
  scale_y_continuous(name = "Number of ratings", labels = scales::comma)
  
# Age of movie vs Stars
edx %>%
  mutate(age = year_rated - year_release) %>% # define age as the age of movie when rated
  filter(age > 0) %>%
  group_by(age) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(age, avg_rating)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Rating based on oldness") +
  xlab("Age") +
  ylab("Stars")


#####################
# Model Preparation #
#####################

## Simplest model: just the mean
mu <- mean(edx$rating)
mu

## Movie effect - b_i
movie_effect <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) 

# visualization
movie_effect %>% 
  ggplot(aes(b_i)) +
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Movie Effect") +
  xlab("Penalty term b_i") +
  ylab("Number of movies")

## User effect - b_u
user_effect <- edx %>% 
  left_join(movie_effect, by = 'movieId') %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i)) 

# visualization
user_effect %>% 
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  ggtitle("User Effect") +
  xlab("Penalty term b_u") +
  ylab("Number of users")

################
# RMSE results #
################

# Define an 'RMSE results' table to store results from various models
rmse_results <- data_frame()

## I. Baseline model 
# Results based on simple prediction according to mu
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

# Add test results to table
rmse_results <- data_frame(method = "Average movie rating model", 
                           RMSE = naive_rmse)
rmse_results %>% knitr::kable()

## II. Movie effect model

# Use validation dataset, predict rating according to movie average
movie_effect_model <- validation %>% 
  left_join(movie_effect, by = 'movieId') %>%
  mutate(pred = mu + b_i) 

# Model results based on just the movie effect
rmse_movie <- RMSE(validation$rating, movie_effect_model$pred)

# Update result table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",  
                                     RMSE = rmse_movie))
rmse_results %>% knitr::kable()

## III. Movie & User effect model

# Use validation dataset, predict rating according to movie & user averages
user_movie_effect_model <- validation %>% 
  left_join(movie_effect, by = 'movieId') %>%
  left_join(user_effect, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) 

# Model results based on movie & user effects
rmse_user_movie <- RMSE(validation$rating, user_movie_effect_model$pred)

# Update result table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie and User Effect Model",  
                                     RMSE = rmse_user_movie))
rmse_results %>% knitr::kable()

## IV. Regularized Movie & User effect model

# Use cross-validation to choose lambda (tuning parameter)
lambdas <- seq(0, 10, 0.25)

# For each lambda, find b_i & b_u, followed by rating prediction & model testing
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  ratings_pred <- validation %>% 
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  rmse <- RMSE(validation$rating, ratings_pred)
  rmse
})

# Plot rmses vs lambdas to select the optimal lambda
rmses <- data.frame(rmse = rmses, lambda = lambdas)
rmses %>%
  ggplot(aes(lambda, rmse)) +
  geom_point() + 
  ggtitle("RMSE based on lambda") +
  ylab("RMSE")

lambda <- rmses$lambda[which.min(rmses$rmse)]
lambda

# Update result table with optimal lambda's model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Regularized Movie and User effect model",  
                                     RMSE = min(rmses$rmse)))
rmse_results %>% knitr::kable()

################
# RMSE results #
################

rmse_results %>% knitr::kable()

############
# Appendix #
############

print("Operating System:")
version