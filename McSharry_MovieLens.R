## THIS CODE WAS PROVIDED BY THE COURSE
###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl,  "ml-10M100K/ratings.dat"))),
col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
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

## BEGINNING OF MCSHARRY'S CODE
# Patricia McSharry - Movielens project 

# install lubridate package if needed
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Function to calculate residual mean squared error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }

# Base model - use the average movie rating of train set to
# predict ratings in test set

# average rating of train set 
mu <- mean(edx$rating)

# calculate RMSE
basic_rmse <- RMSE(validation$rating,mu)
# Store the results in the rmse_results table
rmse_results <- data_frame(model = "Basic Model", RMSE = basic_rmse)

# Movie effect model.  
# Group the train set by movieId
# Train the edx set using the movie effect algorithm 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_movie = mean(rating - mu))

# Using results of training, predict the ratings in the test set
movie_predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_movie)

# Calculate the RMSE 
model_movie_effect <- RMSE(movie_predicted_ratings, validation$rating)
# Store the results in the rmse_results table
rmse_results <- bind_rows(rmse_results,
  data_frame(model="Movie Effect Model",  
  RMSE = model_movie_effect))

# Movie and User Effects Model
# group the edx train set by userID and incorporate the movie effect
# Train the edx set using the movie and userid algorithm
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_user = mean(rating - mu - b_movie))
  
# Using results of training, predict the ratings in the test set
user_movie_predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_movie + b_user) %>%
  pull(pred)

# Calculate the RMSE
model_user_movie_rmse <- RMSE(user_movie_predicted_ratings, validation$rating)
# Store the results in the rmse_results table
rmse_results <- bind_rows(rmse_results,
  data_frame(model="Movie and User Effects Model",  
  RMSE = model_user_movie_rmse))

# Movie, User and Year Difference Effects Model 
			
# Modify the edx and validation sets to add "year" to hold the year the movie was  
# made and to add "yeardiff" to calculate the difference between the year the movie 
# was reviewed and the year it was made.


# modify edx to add year and yeardiff
edx <- edx %>% mutate(year = as.numeric(substring(edx$title,nchar(edx$title)-4,nchar(edx$title)-1)))

edx <- edx %>% mutate(yeardiff = 
as.numeric(substring(as_datetime(edx$timestamp),1,4))-year)

# modify validation to add year and yeardiff
validation <- validation %>% mutate(year = as.numeric(substring(validation$title,nchar(validation$title)-4,nchar(validation$title)-1)))

validation <- validation %>% mutate(yeardiff = 
as.numeric(substring(as_datetime(validation$timestamp),1,4))-year)

# group the edx train set by yeardiff and incorporate the movie and user effects 
# Train the edx set using the movie and userid algorithm
year_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(yeardiff) %>%
  summarize(b_year = mean(rating - mu - b_movie - b_user))
  
# Using results of training, predict the ratings in the test set
year_user_movie_predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='yeardiff') %>%
  mutate(pred = mu + b_user + b_movie + b_year) %>%
  pull(pred)

# Calculate the RMSE

model_year_user_movie_rmse <- RMSE(year_user_movie_predicted_ratings, validation$rating)
# Store the results in the rmse_results table
rmse_results <- bind_rows(rmse_results,
  data_frame(model="Movie, User and Year Difference Effects Model",  
  RMSE = model_year_user_movie_rmse))
# print out the table
rmse_results %>% knitr::kable()

