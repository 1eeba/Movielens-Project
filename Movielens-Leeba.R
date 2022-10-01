if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(caret)
library(data.table)
library(dplyr)

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

head(ratings)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
head(movies)

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

head(movies)

movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)
str(movielens)

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

count(movielens)
count(edx)
count(temp)


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

head(validation)

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

dim(edx)


# Create train and test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Matching userId and movieId in both train and test sets
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Adding back rows into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

dim(edx)
dim(test_set)
dim(train_set)

# Structure and first 6 rows of edx dataset
str(edx)
head(edx)

#unique number of Movies,Users and Genres 

edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId),n_genres=n_distinct(genres))

#Number of movies by genre

genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})





#Top 10 rated movies in the dataset
edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  top_n(10, count) %>%
  arrange(-count) %>%
  ggplot(aes(count, reorder(title, count))) +
  geom_bar(color = "black", fill = "blue", stat = "identity") +
  xlab("Ratings Count") +
  ylab("Movie Name") +
  theme_bw()

# Plot of most common ratings

edx %>% group_by(rating) %>%
  summarise(n_ratings=n()) %>%
  top_n(5, n_ratings) %>%
  arrange(desc(n_ratings))%>%
plot

  
#Rating versus movies:

edx %>% group_by(movieId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "deepskyblue2", bins = 40) +
  xlab("Ratings") +
  ylab("Movies") +
  scale_x_log10() +
  theme_bw()

#Rating versus users

edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "deepskyblue2", bins = 40) +
  xlab("Ratings") +
  ylab("Users") +
  scale_x_log10() +
  theme_bw()

 # Top 10 most rated genres
edx%>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating), num_reviews = n()) %>%
  arrange(desc(avg_rating)) %>% head(10)

#Least 10 rated genres

#Genres verus ratings

edx%>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) + 
  scale_x_log10() +
  geom_histogram(color = "black", fill = "deepskyblue2", bins = 40) + labs(x = 'Number of Ratings', y = 'Number of Genres')


#Evaluation Metric

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Model 1 : Using the mean - RMSE : 1.060054
  
mu <- mean(train_set$rating)
mu

result_model1 <- RMSE(test_set$rating,mu)
result_model1


#Model 2 :Using the median 

med <- median(train_set$rating)
med
result_model2 <- RMSE(test_set$rating,med)
result_model2

#Model 3 :Mean and Movie bias(mb)

mb <- train_set %>%
  group_by(movieId) %>%
  summarize(mb_i = mean(rating - mu))


predicted_ratings <- mu + test_set %>%
  left_join(mb, by = "movieId") %>%
  pull(mb_i)

result_model3 <- RMSE(predicted_ratings, test_set$rating)

result_model3

#Model 4 : Mean(mu),Movie bias(mb),User bias(ub)

ub <- train_set %>%
  left_join(mb, by = "movieId") %>%
  group_by(userId) %>%
  summarize(ub_i = mean(rating - mu - mb_i))


predicted_ratings <- test_set %>%
  left_join(mb, by = "movieId") %>%
  left_join(ub, by = "userId") %>%
  mutate(pred = mu + mb_i + ub_i) %>%
  pull(pred)      

result_model4 <- RMSE(predicted_ratings, test_set$rating)

result_model4

#Model 5 Mean(mu),Movie bias(mb),User bias(ub),Genre bias(gb)


gb <- train_set %>%
  left_join(mb, by = "movieId") %>%
  left_join(ub, by = "userId") %>%
  group_by(genres) %>%
  summarize(gb_i = mean(rating - mu - mb_i - ub_i))


predicted_ratings <- test_set %>%
  left_join(mb, by = "movieId") %>%
  left_join(ub, by = "userId") %>%
  left_join(gb, by = "genres") %>%
  mutate(pred = mu + mb_i + ub_i + gb_i) %>%
  pull(pred)      

result_model5 <- RMSE(predicted_ratings, test_set$rating)

result_model5


#Model 6 : Using lambda

lambdas <- seq(0, 5, 0.25)

#find out the best lambda

rmses <- sapply(lambdas,function(l){
  
  #Calculate the mean of ratings from the edx training set
  mu <- mean(edx$rating)
  
  #Adjust mean by movie bias and penalize low number on ratings
  mb_i <- edx %>% 
    group_by(movieId) %>%
    summarize(mb_i = sum(rating - mu)/(n()+l))
  
  #ajdust mean by user and movie bias and penalize low number of ratings
  mb_u <- edx %>% 
    left_join(mb_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(mb_u = sum(rating - mb_i - mu)/(n()+l))
  
  #predict ratings in the training set to derive optimal penalty value 'lambda'
  predicted_ratings <- 
    edx %>% 
    left_join(mb_i, by = "movieId") %>%
    left_join(mb_u, by = "userId") %>%
    mutate(pred = mu + mb_i + mb_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, edx$rating))
})

plot(lambdas, rmses)
    
min_lambda <- lambdas[which.min(rmses)]
min_lambda

min_rmse <- min(rmses)
min_rmse

result_model6 <- min_rmse
result_model6

#Model 7 :Approach using Matrix Factorization

#Convert data into recosystem format
install.packages('recosystem')
library(recosystem)
set.seed(1, sample.kind="Rounding")
train_reco <- with(train_set, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_reco <- with(test_set, data_memory(user_index = userId, item_index = movieId, rating = rating))
r <- Reco()

para_reco <- r$tune(train_reco, opts = list(dim = c(20, 30),
                                            costp_l2 = c(0.01, 0.1),
                                            costq_l2 = c(0.01, 0.1),
                                            lrate = c(0.01, 0.1),
                                            nthread = 4,
                                            niter = 10))

r$train(train_reco, opts = c(para_reco$min, nthread = 4, niter = 30))
results_reco <- r$predict(test_reco, out_memory())

#Calculate RMSE
result_model7  <- RMSE(results_reco, test_set$rating)

result_model7

#Application to Validation set

set.seed(1, sample.kind="Rounding")
edx_reco <- with(edx, data_memory(user_index = userId, item_index = movieId, rating = rating))
validation_reco <- with(validation, data_memory(user_index = userId, item_index = movieId, rating = rating))
r <- Reco()

para_reco <- r$tune(edx_reco, opts = list(dim = c(20, 30),
                                          costp_l2 = c(0.01, 0.1),
                                          costq_l2 = c(0.01, 0.1),
                                          lrate = c(0.01, 0.1),
                                          nthread = 4,
                                          niter = 10))

r$train(edx_reco, opts = c(para_reco$min, nthread = 4, niter = 30))

final_reco <- r$predict(validation_reco, out_memory())
final_rmse <- RMSE(final_reco, validation$rating)
final_rmse





