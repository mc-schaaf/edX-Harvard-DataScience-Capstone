################################
# Clear workspace, load packages
################################
rm(list = ls())
# you need to load the librarys in this order due to masked functions!
if(!require(scales)) install.packages("scales")
if(!require(data.table)) install.packages("data.table")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(dslabs)) install.packages("dslabs")
if(!require(caret)) install.packages("caret")
if(!require(lubridate)) install.packages("lubridate")
if(!require(recosystem)) install.packages("recosystem")
if(!require(ggthemes)) install.packages("ggthemes")

theme_set(theme_fivethirtyeight()) # change this for different appearance

# Don't forget to adjust your working space!
setwd("C:\\Users\\Moritz\\Desktop\\R_Stuff\\01_HarvardX\\movielens_project")

# If you want the full experience un-comment this. Not recommended.
#do_all_takes_time <- 1









################################
# Create edx set, validation set; protective loop so this process has to be performed only 
# once not always when running this script
################################

if (!("movielens_edx.csv" %in% Sys.glob("*.csv")))
{
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  ratings <-
    fread(
      text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
      col.names = c("userId", "movieId", "rating", "timestamp")
    )
  
  movies <-
    str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <-
    as.data.frame(movies) %>% mutate(
      movieId = as.numeric(levels(movieId))[movieId],
      title = as.character(title),
      genres = as.character(genres)
    )
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  
  # Validation set will be 10% of MovieLens data
  suppressWarnings(set.seed(1, sample.kind = "Rounding"))
  test_index <-
    createDataPartition(
      y = movielens$rating,
      times = 1,
      p = 0.1,
      list = FALSE
    )
  edx <- movielens[-test_index, ]
  temp <- movielens[test_index, ]
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>%
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  
  # Here starts non-edx-code
  # Save the datasets so you dont have to redo the process above every time
  # Delete validation set from workspace so you dont get tempted
  fwrite(edx, "movielens_edx.csv")
  fwrite(validation, "movielens_validation.csv")
  rm(validation)
}

# load local copy of edx dataset
if ("movielens_edx.csv" %in% Sys.glob("*.csv"))
{
  edx <- fread("movielens_edx.csv")
  print("Yay, local loading was sucessful")
}










################################
# Data Exploration
################################

edx %>% names()
edx %>% nrow()/10^6 # 9 million data sets - looks good
unique(edx$userId) %>% length() # ~70 000 users
unique(edx$genres) %>% length() # 797 'unique' genres - this calls for data reduction
unique(edx$rating) %>% .[order(-.)] # range of ratings is 5 to 0.5 in half-star-steps; NO 0 stars are possible
unique(edx$movieId) %>% length() # ~ 10 000 movies
unique(edx$title) %>% length() # not the same - I wonder where this comes from
# why is there more Ids than titles?
edx %>% group_by(movieId, title) %>% count() %>% group_by(title) %>% count() %>% filter(n != 1) # "War of the Worlds (2005)" has 2 movieIds
edx %>% group_by(movieId, title) %>% count() %>% group_by(movieId) %>% count() %>% filter(n != 1) # no movieId has duplicate names
edx %>% filter(title == "War of the Worlds (2005)") %>% group_by(movieId) %>% count() # ok n = 28; this seems to be neglegible.
# has every Id the same genre?
edx %>% group_by(movieId, genres) %>% count() %>% group_by(movieId) %>% count() %>% filter(n != 1) # okay every Id has only one genre applied
# what are the top genres and the genres not used too often?
edx %>% group_by(movieId, genres) %>% count() %>% group_by(genres) %>% count() %>% arrange(n) %>% head()
edx %>% group_by(movieId, genres) %>% count() %>% group_by(genres) %>% count() %>% arrange(desc(n)) %>% head() # it looks like the too specific genres are the reason for small "unique" genres












################################
# Data Visualization
################################
# again protective if-statement so this is not run every time
if(exists("do_all_takes_time")){
  
  # define function that makes readable exponents
  readable_exponents <- function(l) {
    # turn in to character string in scientific notation
    l <- format(l, scientific = TRUE)
    l <- gsub("0e\\+00", "0", l)
    # quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    # turn the 'e+' into plotmath format
    l <- gsub("e\\+", "e", l)
    l <- gsub("e", "%*%10^", l)
    l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
    # return this as an expression
    parse(text = l)
  }
  
  
  # inspect rating distribution
  edx %>% ggplot() + geom_histogram(aes(x = rating), binwidth = 0.25) +
    geom_vline(xintercept = mean(edx$rating), color = "red") +
    ggtitle("Distribution of ratings", subtitle = "with mean") +
    scale_y_continuous(labels = readable_exponents)
  print(last_plot())
  # full-star ratings are more often than half-star ratings; skewed distribution
  
  
  users_n <- edx %>% group_by(userId) %>% count() 
  users_n %>% ggplot() + geom_histogram(aes(x = n), bins = 50) + 
    geom_vline(xintercept = quantile(users_n$n, probs = c(0.25, 0.5, 0.75)), color = "red") + 
    geom_label(data = data.frame(x = c(1, 1, 1), y = c( 1, 1, 1)), 
               aes(x = quantile(users_n$n, probs = c(0.25, 0.5, 0.75)), y = rep(4000, 3), 
                   label = paste0("n =\n", quantile(users_n$n, probs = c(0.25, 0.5, 0.75))), color = "red")) +
    scale_x_continuous(trans = "log10", labels = readable_exponents) + theme(legend.position = "none") +
    scale_y_continuous(labels = readable_exponents) +
    ggtitle("Distribution of number of ratings per user", subtitle = "with 25%, 50% and 75% quantiles")
  print(last_plot())
  # most users have between 25 and 150 ratings; however, there are some users with 6000+ ratings

    
  movies_n <- edx %>% group_by(movieId) %>% count() 
  movies_n %>% ggplot() + geom_histogram(aes(x = n), bins = 50) + 
    geom_vline(xintercept = quantile(movies_n$n, probs = c(0.25, 0.5, 0.75)), color = "red") + 
    geom_label(data = data.frame(x = c(1, 1, 1), y = c( 1, 1, 1)), 
               aes(x = quantile(movies_n$n, probs = c(0.25, 0.5, 0.75)), y = rep(400, 3), 
                   label = paste0("n =\n", quantile(movies_n$n, probs = c(0.25, 0.5, 0.75))), color = "red")) +
    scale_x_continuous(trans = "log10", labels = readable_exponents) + theme(legend.position = "none") + 
    ggtitle("Distribution of number of ratings per movie", subtitle = "with 25%, 50% and 75% quantiles")
  print(last_plot())
  # most movies have between 25 and 1000 ratings; looks normally distributed

  
  genres_n <- edx %>% group_by(genres) %>% count() 
  genres_n %>% ggplot() + geom_histogram(aes(x = n), bins = 50) + 
    geom_vline(xintercept = quantile(genres_n$n, probs = c(0.25, 0.5, 0.75)), color = "red") + 
    geom_label(data = data.frame(x = c(1, 1, 1), y = c( 1, 1, 1)), 
               aes(x = quantile(genres_n$n, probs = c(0.25, 0.5, 0.75)), y = rep(35, 3), 
                   label = paste0("n =\n", quantile(genres_n$n, probs = c(0.25, 0.5, 0.75))), color = "red")) +
    scale_x_continuous(trans = "log10", labels = readable_exponents) + theme(legend.position = "none") + 
    ggtitle("Distribution of number of ratings per genre", subtitle = "with 25%, 50% and 75% quantiles")
  print(last_plot())
  
  
  genres_n <- edx %>% group_by(genres, movieId) %>% count() %>% group_by(genres) %>% count()
  genres_n %>% ggplot() + geom_histogram(aes(x = n, y = ifelse(..count..+0.1 > 0.1,..count..+0.1, 0)), bins = 50) + 
    geom_vline(xintercept = quantile(genres_n$n, probs = c(0.25, 0.5, 0.75)), color = "red") + 
    geom_label(data = data.frame(x = c(1, 1, 1), y = c( 1, 1, 1)), 
               aes(x = quantile(genres_n$n, probs = c(0.25, 0.5, 0.75)), y = rep(50, 3), 
                   label = paste0("n =\n", quantile(genres_n$n, probs = c(0.25, 0.5, 0.75))), color = "red")) +
    scale_x_continuous(trans = "log10", labels = readable_exponents) + 
    theme(legend.position = "none") + 
    ggtitle("Distribution of number of movies per genre", subtitle = "with 25%, 50% and 75% quantiles")
  print(last_plot())
}
# most genres (old & unique) have between 150 and 7500 ratings; looks normally distributed
# that is too few ratings for stable estimations of genres that go beyond movie effects
# which can also be seen as more than 75% of genres have 5 or less movies in them









################################
# Data wrangling
################################
# To Do:
# change movie Id "64997" to "34048" (same film but multiple Ids)
# extract release date from title
# wrange genres-column to more sensible columns
# wrangle timestamp-column to multiple columns



# Change movieId of "War of the Worlds (2005)"
edx <- edx %>% mutate( 
     movieId = ifelse(movieId == 64997, 34048, movieId)
)
if (unique(edx$movieId) %>% length() != unique(edx$title) %>% length()) {stop("movieId broke")}
# worked





# wrange movietitle-column 
# make a small subset that contains only 1 rating per movie
# this preserves the original data format but makes computation faster (I'm working on a really old laptop, sorry!)
one_per_film <- edx %>% group_by(title) %>% top_n(1, timestamp)

# set up extraction pattern
year_pattern <- "\\([0123456789]{4}\\)$"
str_view(one_per_film$title[1:19], year_pattern)

# Is the pattern exclusive for the year or is there a similar pattern in the title?
# And is this pattern in every movie?
one_per_film %>% mutate(year_n = str_count(title, year_pattern)) %>% filter(year_n != 1)
# The year pattern seems to workas the tibble is empty

# Use pattern
one_per_film <- one_per_film %>% ungroup() %>% mutate(
  year = str_extract(title, year_pattern) %>% str_remove_all("\\(|\\)") %>% as.integer(),
  title = str_remove(title, paste0(" ", year_pattern))
) 
head(one_per_film)
if(any(one_per_film$year < 1900 | one_per_film$year > 2020 | is.na(one_per_film$year))) {stop("year extraction did not work properly")}
# seems to have worked

# shorten data frames
# Then merge data frames
edx <- edx %>% select(-title)
one_per_film <- one_per_film %>% select(-c(userId, timestamp, genres, rating))

edx <- full_join(edx, one_per_film, by = c("movieId"))
any(is.na(edx))
head(edx)
rm(year_pattern, one_per_film)
# year extraction worked



# wrangle timestamp of rating
edx <- edx %>% mutate(
  rating_year = year(as_datetime(timestamp)),
  rating_day = lubridate::wday(as_datetime(timestamp), label = TRUE, locale = "US"),
  rating_hour = hour(as_datetime(timestamp))
)





# wrange genres-column 
# make a small subset that contains only 1 rating per unique sting in the genres-column
# this preserves the original data format but makes computation faster
one_per_genres <- edx %>% group_by(genres) %>% top_n(1, timestamp)

# extract all the unique genres
unique_genres <- one_per_genres$genres %>% str_split("\\|") %>% unlist() %>% unique()
unique_genres

# check if it worked
one_per_genres$genres %>% str_remove_all(paste(unique_genres, collapse = "|")) %>% str_remove_all("\\|") %>% unique()
# okay it did not get the brackets of ""(no genres listed)" due to missing escape sequences. I can live with that.

# Maybe the first genre listed is the main genre?
one_per_genres <- one_per_genres %>% mutate(main_genre = str_split_fixed(genres, "\\|", n = 2)[1])

# produce a logical matrix-like object if genre XY is in the genres column
for (i in 1:length(unique_genres)) {
  one_per_genres$V1 <- str_detect(one_per_genres$genres, unique_genres[i])
  one_per_genres <- one_per_genres %>% rename(!!unique_genres[i] := "V1")
}
rm(i)

# shorten data frames
# Then merge data frames
one_per_genres <- one_per_genres %>% select(-c(userId, movieId, rating, timestamp, title, year, rating_day, rating_year, rating_hour))

# edx <- full_join(edx, one_per_genres, by = c("genres"))
# any(is.na(edx))
# head(edx)
# rm(unique_genres, one_per_genres)
# genre-wrangling seems to have worked














################################
# Split into test and training set
################################

# Test set will be 10% of edx data
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
tmp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
edx_test <- tmp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(tmp, edx_test)
edx_train <- rbind(edx_train, removed)
if(nrow(edx) != nrow(edx_test) + nrow(edx_train)) {stop("Splitting into test/trains seems to have failed")}

rm(edx, removed, tmp, test_index)

# make lists containing x and y 
y <- edx_train %>% pull(rating)
x <- edx_train %>% select(-rating)
edx_train <- list(y = y, x = x)

y <- edx_test %>% pull(rating)
x <- edx_test %>% select(-rating)
edx_test <- list(y = y, x = x)

rm(x,y)










################################
# Baseline Measurements
################################
# define evaluation function
fun_RMSE <- function(y_hat) {
  RSS <- sum((y_hat - edx_test$y)^2)
  RMSE <- sqrt(RSS/length(y_hat))
  return(RMSE)
}
# goal: RMSE < 0.86490

# open up dataframe to store the RSME results
RMSE_results <- data.frame(Method = NA, RMSE = NA)

# If we knew nothing at all about the edx_train set, we would guess every star-category with equal probability
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
possible_outcomes <- c((1:10)/2)
y_hat <- sample(possible_outcomes, size = length(edx_test$y), replace = TRUE)
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results[1,] <- c("Guessing with equal probabilities", tmp_RMSE)
tmp_RMSE
# In this scenario, we produce an average error of about 2


# If we could also guess with probabilities representing the estimated probabilites
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
possible_outcomes <- c((1:10)/2)
probs <- sapply(possible_outcomes, function(x){mean(x == edx_train$y)})
y_hat <- sample(possible_outcomes, size = length(edx_test$y), replace = TRUE, prob = probs)
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("Guessing with estimated probabilities", tmp_RMSE))
tmp_RMSE
rm(probs, possible_outcomes)
# We manage to decrease the RMSE by 0.5 


# Still not knowing anything about predictors, we could always guess the estimated average rating
mu <- mean(edx_train$y)
y_hat <- rep(mu, times = length(edx_test$y))
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("Guessing the estimated mean", tmp_RMSE))
tmp_RMSE
# We - again - manage to decrease the RSME by 0.5 to about 1.1. Still we have not used any predictors











################################
# Movie and User effects - unregularized version 
################################

# estimate movie effects without regularization
movie_bs <-
  cbind(edx_train$x, edx_train$y) %>% rename("true_y" = "edx_train$y") %>%
  group_by(movieId) %>%
  summarize(b_movie = mean(true_y - mu))

# apply movie effects and compute RSME
predictions <- data.frame(
  mu = mu,
  b_movie = edx_test$x %>%
    left_join(movie_bs, by = "movieId") %>%
    pull(b_movie)
)
y_hat <- predictions %>% rowSums()
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("Unregulated movie effects", tmp_RMSE))
tmp_RMSE



# estimate user effects without regularization
user_bs <- cbind(edx_train$x, edx_train$y) %>% 
  rename("true_y" = "edx_train$y") %>%
  left_join(movie_bs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_user = mean(true_y - (mu + b_movie)))

# apply user effects and compute RSME
predictions$b_user <- edx_test$x %>%
  left_join(user_bs, by = "userId") %>%
  pull(b_user)
y_hat <- predictions %>% rowSums()
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("Unregulated movie and user effects", tmp_RMSE))
tmp_RMSE

# The unregularized version works better than in the example from the course material.
# This is to be expected, as we have a much larger data basis than the course material, thus reducing "noise" by larger samples
# However, maybe penaltizing small sample sizes still shrinks our RMSE?













################################
# Movie and User effects - regularized version 
################################

## Inspect different lambdas in the train sample - movie effect
# Protective wrapper so this is not run every time - I have a really slow laptop sorry! 
if (exists("do_all_takes_time")) {
  # some sensible lambda values
  lambdas <- seq(0, 10, 0.25)
  # 100 data partitions; overlapping
  suppressWarnings(set.seed(1, sample.kind = "Rounding"))
  RS_index <- createDataPartition(edx_train$y, times = 100, p = 0.1)
  # create a funktion that gives RSMEs conditional on lambdas
  fun_regularization <- function(lambdas, index) {
    # create data partitions
    data_small <-
      cbind(edx_train$y, edx_train$x) %>% rename("true_y" = "edx_train$y") %>% select(true_y, movieId)
    data_train <- data_small[-index,]
    data_test_tmp <- data_small[index,]
    # Make sure movieId is in both sets
    data_test <- data_test_tmp %>%
      semi_join(data_train, by = "movieId")
    removed <- anti_join(data_test_tmp, data_test)
    data_train <- rbind(data_train, removed)
    # create everything where lambda is not needed
    tmp_mu <- mean(data_train$true_y)
    just_the_sum <- data_train %>%
      group_by(movieId) %>%
      summarize(s = sum(true_y - tmp_mu), n_i = n())
    # create the RMSE conditional on the lambda
    rmses <- sapply(lambdas, function(l) {
      predicted_ratings <- data_test %>%
        left_join(just_the_sum, by = 'movieId') %>%
        mutate(b_i = s / (n_i + l)) %>%
        mutate(pred = tmp_mu + b_i) %>%
        pull(pred)
      RMSE <- sqrt(mean((
        predicted_ratings - data_test$true_y
      ) ^ 2))
      return(RMSE)
    })
    # give back only the RMSEs
    return(rmses)
  }
  
  # apply this function 100 times
  lambda_RMSEs <- lapply(RS_index, function(x) {
    fun_regularization(lambdas, x)
  })
  
  # inspect the results
  lambda_RMSE_mean <-
    lambda_RMSEs %>% as.data.frame() %>% rowMeans()
  lambda_RMSE_sd <-
    lambda_RMSEs %>% as.data.frame %>% as.matrix() %>% rowSds()
  ggplot(data = NULL) + geom_point(aes(lambdas, lambda_RMSE_mean)) +
    geom_errorbar(aes(
      x = lambdas,
      ymin = (lambda_RMSE_mean - lambda_RMSE_sd / sqrt(length(lambda_RMSEs))),
      ymax = (lambda_RMSE_mean + lambda_RMSE_sd / sqrt(length(lambda_RMSEs)))
    )) +
    ggtitle("RMSE conditional on Lambda", subtitle = "movie effect, cross-validation, errorbars indicate SE")
  print(last_plot())
  lambdas[which.min(lambda_RMSE_mean)]
  # optimal lambda is 2.25 in train set
  rm(RS_index, lambdas, lambda_RMSEs)
}


## Apply optimal lambda to whole edx_test and update movie effects
# I set lambda manually to the optimum as the function above takes very much time
movie_lambda <- 2.25

# estimate movie effects with regularization
movie_bs <-
  cbind(edx_train$x, edx_train$y) %>% rename("true_y" = "edx_train$y") %>%
  group_by(movieId) %>%
  summarize(s = sum(true_y - mu),
            n_i = n()) %>%
  mutate(b_movie = s / (n_i + movie_lambda)) %>%
  select(movieId, b_movie)

# apply movie effects and compute RSME
predictions <- data.frame(
  mu = mu,
  b_movie = edx_test$x %>%
    left_join(movie_bs, by = "movieId") %>%
    pull(b_movie)
)
y_hat <- predictions %>% rowSums()
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("Regulated movie effects", tmp_RMSE))
tmp_RMSE




## Inspect different lambdas in the train sample - user effect
# Protective wrapper so this is not run every time - I have a really slow laptop sorry!
if (exists("do_all_takes_time")) {
  # some sensible lambda values
  lambdas <- seq(0, 10, 0.25)
  # 100 data partitions; overlapping
  suppressWarnings(set.seed(1, sample.kind = "Rounding"))
  RS_index <- createDataPartition(edx_train$y, times = 100, p = 0.1)
  data_small <-
    cbind(edx_train$y, edx_train$x) %>% rename("true_y" = "edx_train$y") %>%
    left_join(movie_bs, by = "movieId") %>% mutate(residual = true_y - (mu + b_movie)) %>% select(residual, userId)
  # create a funktion that gives RSMEs conditional on lambdas
  fun_regularization <- function(lambdas, index) {
    # create data partitions
    data_train <- data_small[-index, ]
    data_test_tmp <- data_small[index, ]
    # Make sure userId is in both sets
    data_test <- data_test_tmp %>%
      semi_join(data_train, by = "userId")
    removed <- anti_join(data_test_tmp, data_test)
    data_train <- rbind(data_train, removed)
    # create everything where lambda is not needed
    tmp_mu <- mean(data_train$residual)
    just_the_sum <- data_train %>%
      group_by(userId) %>%
      summarize(s = sum(residual - tmp_mu), n_i = n())
    # create the RMSE conditional on the lambda
    rmses <- sapply(lambdas, function(l) {
      predicted_ratings <- data_test %>%
        left_join(just_the_sum, by = 'userId') %>%
        mutate(b_i = s / (n_i + l)) %>%
        mutate(pred = tmp_mu + b_i) %>%
        pull(pred)
      RMSE <- sqrt(mean((
        predicted_ratings - data_test$residual
      ) ^ 2))
      return(RMSE)
    })
    # give back only the RMSEs
    return(rmses)
  }
  # apply this function 100 times
  lambda_RMSEs <- lapply(RS_index, function(x) {
    fun_regularization(lambdas, x)
  })
  # inspect the results
  lambda_RMSE_mean <-
    lambda_RMSEs %>% as.data.frame() %>% rowMeans()
  lambda_RMSE_sd <-
    lambda_RMSEs %>% as.data.frame %>% as.matrix() %>% rowSds()
  ggplot(data = NULL) + geom_point(aes(lambdas, lambda_RMSE_mean)) +
    geom_errorbar(aes(
      x = lambdas,
      ymin = (lambda_RMSE_mean - lambda_RMSE_sd / sqrt(length(lambda_RMSEs))),
      ymax = (lambda_RMSE_mean + lambda_RMSE_sd / sqrt(length(lambda_RMSEs)))
    )) +
    ggtitle("RMSE conditional on Lambda", subtitle = "user effect, cross-validation, errorbars indicate SE")
  print(last_plot())
  lambdas[which.min(lambda_RMSE_mean)]
  # optimal lambda is 5.00 in train set
  rm(RS_index, lambdas, lambda_RMSEs)
}


## Apply optimal lambda to whole edx_test and update movie effects
# I set lambda manually to the optimum as the function above takes very much time
user_lambda <- 5

# estimate user effects with regularization
user_bs <-
  cbind(edx_train$x, edx_train$y) %>% rename("true_y" = "edx_train$y") %>%
  left_join(movie_bs, by = "movieId") %>% mutate(residual = true_y - (mu + b_movie)) %>%
  group_by(userId) %>%
  summarize(s = sum(residual),
            n_i = n()) %>%
  mutate(b_user = s / (n_i + user_lambda)) %>%
  select(userId, b_user)



# apply user effects and compute RSME
predictions$b_user <- edx_test$x %>%
  left_join(user_bs, by = "userId") %>%
  pull(b_user)
y_hat <- predictions %>% rowSums()
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <-
  rbind(RMSE_results, c("Regulated movie and user effects", tmp_RMSE))
tmp_RMSE
# With knowledge about user and movie averages, we managed to reduce the RMSE to 0.865
range(y_hat)
y_hat_clipped <-
  ifelse(y_hat > 5, 5, ifelse(y_hat < 0.5, 0.5, y_hat))
tmp_RMSE <- fun_RMSE(y_hat_clipped)
RMSE_results <-
  rbind(RMSE_results,
        c("Regulated movie and user effects, clipped", tmp_RMSE))
tmp_RMSE
rm(y_hat_clipped)
# clipping can slightly reduce the RSME
# This was the goal set earlier.
# However, better save than sorry.
# We have to try and further decrease the RMSE






################################
# Simple genre effects und genre-user Interaction
################################

# We could try and use an additive genre effect


## Inspect different lambdas in the train sample - genre effect
# Protective wrapper so this is not run every time - I have a really slow laptop sorry!
if (exists("do_all_takes_time")) {
  # some sensible lambda values
  lambdas <- seq(0, 10000, 100)
  # 100 data partitions; overlapping
  suppressWarnings(set.seed(1, sample.kind = "Rounding"))
  RS_index <- createDataPartition(edx_train$y, times = 100, p = 0.1)
  data_small <-
    cbind(edx_train$y, edx_train$x) %>% rename("true_y" = "edx_train$y") %>%
    left_join(movie_bs, by = "movieId") %>%
    left_join(user_bs, by = "userId") %>%
    left_join(one_per_genres, by = "genres") %>% mutate(residual = true_y - (mu + b_movie + b_user)) %>% select(residual, main_genre)
  # create a funktion that gives RSMEs conditional on lambdas
  fun_regularization <- function(lambdas, index) {
    # create data partitions
    data_train <- data_small[-index, ]
    data_test_tmp <- data_small[index, ]
    # Make sure genre is in both sets
    data_test <- data_test_tmp %>%
      semi_join(data_train, by = "main_genre")
    removed <- anti_join(data_test_tmp, data_test)
    data_train <- rbind(data_train, removed)
    # create everything where lambda is not needed
    tmp_mu <- mean(data_train$residual)
    just_the_sum <- data_train %>%
      group_by(main_genre) %>%
      summarize(s = sum(residual - tmp_mu), n_i = n())
    # create the RMSE conditional on the lambda
    rmses <- sapply(lambdas, function(l) {
      predicted_ratings <- data_test %>%
        left_join(just_the_sum, by = 'main_genre') %>%
        mutate(b_i = s / (n_i + l)) %>%
        mutate(pred = tmp_mu + b_i) %>%
        pull(pred)
      RMSE <- sqrt(mean((
        predicted_ratings - data_test$residual
      ) ^ 2))
      return(RMSE)
    })
    # give back only the RMSEs
    return(rmses)
  }
  # apply this function 100 times
  lambda_RMSEs <- lapply(RS_index, function(x) {
    fun_regularization(lambdas, x)
  })
  # inspect the results
  lambda_RMSE_mean <-
    lambda_RMSEs %>% as.data.frame() %>% rowMeans()
  lambda_RMSE_sd <-
    lambda_RMSEs %>% as.data.frame %>% as.matrix() %>% rowSds()
  ggplot(data = NULL) + geom_point(aes(lambdas, lambda_RMSE_mean)) +
    geom_errorbar(aes(
      x = lambdas,
      ymin = (lambda_RMSE_mean - lambda_RMSE_sd / sqrt(length(lambda_RMSEs))),
      ymax = (lambda_RMSE_mean + lambda_RMSE_sd / sqrt(length(lambda_RMSEs)))
    )) +
    ggtitle("RMSE conditional on Lambda", subtitle = "genre effect, cross-validation, errorbars indicate SE")
  print(last_plot())
  lambdas[which.min(lambda_RMSE_mean)]
  # optimal lambda is 1800 in train set
  rm(RS_index, lambdas, lambda_RMSEs)
}
## Apply optimal lambda to whole edx_test and estimate genre effects
# I set lambda manually to the optimum as the function above takes very much time
genre_lambda <- 1800

# estimate genre effects with regularization
genre_bs <-
  cbind(edx_train$x, edx_train$y) %>% rename("true_y" = "edx_train$y") %>%
  left_join(movie_bs, by = "movieId") %>%
  left_join(user_bs, by = "userId") %>%
  left_join(one_per_genres, by = "genres") %>% 
  mutate(residual = true_y - (mu + b_movie + b_user)) %>%
  group_by(main_genre) %>%
  summarize(s = sum(residual),
            n_i = n()) %>%
  mutate(b_genre = s / (n_i + genre_lambda)) %>%
  select(main_genre, b_genre)

# apply genre effects and compute RSME
predictions$b_genre <- edx_test$x %>% 
  left_join(one_per_genres, by = "genres") %>%
  left_join(genre_bs, by = "main_genre") %>%
  pull(b_genre)
y_hat <- predictions %>% rowSums()
y_hat <- ifelse(y_hat > 5, 5, ifelse(y_hat < 0.5, 0.5, y_hat))
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("+ genre effect", tmp_RMSE))
tmp_RMSE




# The RMSE does not decrease very much. This is not surprising, as we explained much of the genre-variance with 
# the movie effects. One could try to predict the ratings by e.g. the time of day, the year, or the weekday of the rating.
# However, the task is not named "rating prediction" but "movie recommendation".
# Until now, every person would get the same recommendation but with different expected rating.
# So we try to predict the entertainment of a movie for a person, given the rating of similar and dissimilar movies
# The most simple approach is a genre-effect that is not global but for an specific individual:

## Inspect different lambdas in the train sample - genre-user interaction
# Protective wrapper so this is not run every time - I have a really slow laptop sorry!
if (exists("do_all_takes_time")) {
  # some sensible lambda values
  lambdas <- seq(0, 100, 2.5)
  # 100 data partitions; overlapping
  suppressWarnings(set.seed(1, sample.kind = "Rounding"))
  RS_index <- createDataPartition(edx_train$y, times = 100, p = 0.1)
  data_small <-
    cbind(edx_train$y, edx_train$x) %>% rename("true_y" = "edx_train$y") %>%
    left_join(movie_bs, by = "movieId") %>%
    left_join(user_bs, by = "userId")  %>%
    left_join(one_per_genres, by = "genres") %>%
    left_join(genre_bs, by = "main_genre") %>% mutate(residual = true_y - (mu + b_movie + b_user + b_genre)) %>% select(residual, main_genre, userId)
  # create a funktion that gives RSMEs conditional on lambdas
  fun_regularization <- function(lambdas, index) {
    # create data partitions
    data_train <- data_small[-index, ]
    data_test_tmp <- data_small[index, ]
    # Make sure genre is in both sets
    data_test <- data_test_tmp %>%
      semi_join(data_train, by = c("main_genre", "userId"))
    removed <- anti_join(data_test_tmp, data_test)
    data_train <- rbind(data_train, removed)
    # create everything where lambda is not needed
    tmp_mu <- mean(data_train$residual)
    just_the_sum <- data_train %>%
      group_by(main_genre, userId) %>%
      summarize(s = sum(residual - tmp_mu), n_i = n())
    # create the RMSE conditional on the lambda
    rmses <- sapply(lambdas, function(l) {
      predicted_ratings <- data_test %>%
        left_join(just_the_sum, by = c("main_genre" , "userId")) %>%
        mutate(b_i = s / (n_i + l)) %>%
        mutate(pred = tmp_mu + b_i) %>%
        pull(pred)
      RMSE <- sqrt(mean((
        predicted_ratings - data_test$residual
      ) ^ 2))
      return(RMSE)
    })
    # give back only the RMSEs
    return(rmses)
  }
  # apply this function 100 times
  lambda_RMSEs <- lapply(RS_index, function(x) {
    fun_regularization(lambdas, x)
  })
  # inspect the results
  lambda_RMSE_mean <-
    lambda_RMSEs %>% as.data.frame() %>% rowMeans()
  lambda_RMSE_sd <-
    lambda_RMSEs %>% as.data.frame %>% as.matrix() %>% rowSds()
  ggplot(data = NULL) + geom_point(aes(lambdas, lambda_RMSE_mean)) +
    geom_errorbar(aes(
      x = lambdas,
      ymin = (lambda_RMSE_mean - lambda_RMSE_sd / sqrt(length(lambda_RMSEs))),
      ymax = (lambda_RMSE_mean + lambda_RMSE_sd / sqrt(length(lambda_RMSEs)))
    )) +
    ggtitle("RMSE conditional on Lambda", subtitle = "interaction effect, cross-validation, errorbars indicate SE")
  print(last_plot())
  lambdas[which.min(lambda_RMSE_mean)]
  # optimal lambda is 22.5 in train set
  rm(RS_index, lambdas, lambda_RMSEs)
}

## Apply optimal lambda to whole edx_test and estimate genre effects
# I set lambda manually to the optimum as the function above takes very much time
IA_lambda <- 22.5

# estimate genre-user interaction with regularization
IA_bs <-
  cbind(edx_train$x, edx_train$y) %>% rename("true_y" = "edx_train$y") %>%
  left_join(movie_bs, by = "movieId") %>%
  left_join(user_bs, by = "userId") %>%
  left_join(one_per_genres, by = "genres") %>%
  left_join(genre_bs, by = "main_genre") %>% mutate(residual = true_y - (mu + b_movie + b_user + b_genre)) %>%
  group_by(userId, main_genre) %>%
  summarize(s = sum(residual),
            n_i = n()) %>%
  mutate(b_IA = s / (n_i + IA_lambda)) %>%
  select(userId, main_genre, b_IA)

# apply genre-user interaction and compute RSME
predictions$b_IA <- edx_test$x %>% 
  left_join(one_per_genres, by = "genres") %>%
  left_join(IA_bs, by = c("main_genre", "userId")) %>%
  mutate(b_IA = ifelse(is.na(b_IA), 0, b_IA)) %>%
  pull(b_IA)
y_hat <- predictions %>% rowSums()
y_hat <- ifelse(y_hat > 5, 5, ifelse(y_hat < 0.5, 0.5, y_hat))
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("+ genre - user interaction", tmp_RMSE))
tmp_RMSE


# We see that this decreases the RSME substantially.
# The residuals should be uncorrelated now, right?
# Let's take a look!












################################
# Matrix factorization - demonstration
################################

# create a dataset that only contains userId, main genre and residuals - from the test not train set
genres_small <- edx_test$x %>% left_join(one_per_genres, by = "genres") %>% select(userId, main_genre, genres)
genres_small$residuals <- edx_test$y - (predictions %>% select(-b_IA) %>% rowSums())
  
# Keep only genres and users that have many ratings
genres_small <- genres_small %>%
  group_by(userId, main_genre) %>%
  summarize(residuals = mean(residuals)) %>%
  ungroup() %>%
  mutate(main_genre = as.factor(main_genre)) %>%
  group_by(main_genre) %>%
  filter(n() >= 500)%>%
  group_by(userId) %>%
  filter(n() >= 10)

# convert it to a matrix where every row represents a user and every column a genre Values are the residuals
y <- genres_small %>% select(main_genre, residuals, userId) %>%
  pivot_wider(names_from = main_genre, values_from = residuals) %>% as.matrix()
rownames(y)<- y[,1]
y <- y[,-1]

# We see that there is some substantial correlation between the residuals of the genres
cor(y, use = "pairwise.complete.obs") %>% heatmap(col = RColorBrewer::brewer.pal(5, "Greys"), symm = TRUE)
# This is surprising, hadn't we adjusted for the genre effekt?
# Not quite, as we had a very large lambda for the user-genre interaction
# Therefore, small genres like Sci-Fi and Fantasy are not explained completely. 
# Sci-Fi-ness is not dichotomous, but rather a continuum. The same holds true for Fantasy,
# and some movies incorporate aspects of both genres.
# However, our hands-on approach does ony allow for each movie to be in one genre.
# We reduced the dimensions of the genres drastically!

# We could also try to reduce the dimensions by factor analysis or principal component analysis
# The main genre was a good start so let's work with the residuals again.
# However, we face a lot of missing data. Black indicates data, white indicates missing data
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
ifelse(!is.na(y), 1, 0) %>% .[sample(1:nrow(y), 50),] %>% .[, order(-colSums(.))] %>%
  t() %>% image(1:17, 1:50, ., xlab = "Genres", ylab = "50 random users", col = RColorBrewer::brewer.pal(5, "Greys"))
# abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# One could replace missing values with 0. However, this decreases the variance of genres that have 
# only few ratings. Those are the genres that we are especially interested in.
# Therefore, let's try some multiple imputation and then Principal Component Analysis
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
pca_md <- missMDA::imputePCA(y, ncp = 3)
y_md <- pca_md$completeObs
pca <- prcomp(y_md)

# make plot for variability explained
ggplot(aes(1:length(pca$sdev), (pca$sdev^2 / sum(pca$sdev^2))), data = NULL) + geom_point() +
  scale_y_continuous(name = "variance explained", limits = c(0,0.3)) + xlab("PC") +
  ggtitle("Variance explained by Principal Components")
# We see that there seems to be a little structure left in the data!

# visualize the structure 
pca$rotation %>% as.data.frame() %>% mutate(genres = rownames(.)) %>% 
  ggplot(aes(PC1, PC2, label = genres)) + geom_point() + ggrepel::geom_text_repel(size = 3) +
  ggtitle("Movies", subtitle = "on PC1 and PC2")
# PC1: Sci-Fi/ Fantasy vs. Musical/ Romance
# PC2: Kid's movies vs. adult movies
# We see the concept behind matrix factorization. We try to explain the matrix (= the ratings) by searching 
# for similarities between users and similarities between genres/movies















################################
# Applying the concepts with the recosystem package
################################
# We can also extend this concept of "guessing" the factor structure to movies instead of genres as columns.
# However, the computational reqirements of such an operation exceed the hardware specifications of 
# my standard, portable computer (especially working memory). Therefore, to use the concept just described,
# I will use the "recosystem" package. The operations are directly implemented in C++ and allow to write objects on 
# the hard drive directly, thus lowering the system requirements


# initialize reco
r = Reco()

# create a dataset with only rating, userId and movieId
reco_train <- edx_train$x %>% cbind(edx_train$y) %>% rename("y" = "edx_train$y") %>%
  left_join(movie_bs, by = "movieId") %>% 
  left_join(user_bs, by = "userId") %>% 
  left_join(one_per_genres, by = "genres") %>%
  left_join(genre_bs, by = "main_genre") %>%
  mutate(residual = y - (mu + b_movie + b_user + b_genre)) %>%
  select(userId, movieId, y)

# reco wants a special data format
reco_train <-  with(reco_train, 
                    data_memory(user_index = userId, item_index = movieId, rating = y, index1 = TRUE))


# try some tuning parameters, 3-fold cross validation is enough
# again protective wrapper
if (1 == 2){
  opts = r$tune(
    reco_train,
    opts = list(
      dim = c(5, 10, 20, 50),
      lrate = c(0.1, 0.2),
      costp_l1 = c(0),
      costq_l1 = c(0),
      costp_l2 = c(0, 0.01, 0.1, 0.3),
      costq_l2 = c(0, 0.01, 0.1, 0.3),
      nthread = 4,
      niter = 20,
      nfold = 3
    )
  )
  opts$min
}

# I set this by hand as the computation above takes hours on my device.
# Go ahead and give it a try if you possess a modern stationary PC. But don't forget to set the number of threads accordingly
best_options <- list(
  dim = 50,
  lrate = 0.1,
  costp_l1 = 0,
  costq_l1 = 0,
  costp_l2 = 0.01,
  costq_l2 = 0.1,
  nthread = 4
)

# train the model
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
r$train(reco_train, opts = c(best_options, niter = 50, verbose = FALSE))

# create evaluation dataset
reco_test <- edx_test$x %>% select(userId, movieId)
reco_test <-  with(reco_test, 
                   data_memory(user_index = userId, item_index = movieId, index1 = TRUE))

# create predictions and test them
y_hat <- r$predict(reco_test, out_memory())
y_hat <- ifelse(y_hat > 5, 5, ifelse(y_hat < 0.5, 0.5, y_hat))
tmp_RMSE <- fun_RMSE(y_hat)
RMSE_results <- rbind(RMSE_results, c("reco package", tmp_RMSE))
tmp_RMSE



















################################
# Wrap-Up: Applying best model to whole edx set and compute validation RMSE
################################
# clean up everything unnecessary
workspace <- ls()
rm(list = workspace[!workspace %in% c("best_options", "RMSE_results")])

# read in validation set (which was unknown before)
edx <- fread("movielens_edx.csv") %>% select(userId, movieId, rating)
validation_predictors <- fread("movielens_validation.csv") %>% select(userId, movieId)

# initialize Reco and wrangle data sets
r = Reco()
edx <-  with(edx, data_memory(user_index = userId, item_index = movieId, rating = rating, index1 = TRUE))
validation_predictors <-  with(validation_predictors, 
                               data_memory(user_index = userId, item_index = movieId, index1 = TRUE))

# train the reco system
# if running this: consider lower number of iterations to speed things up
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
r$train(edx, opts = c(best_options, niter = 1000, verbose = FALSE))

# predict ratings
y_hat <- r$predict(validation_predictors, out_memory())
y_hat <- ifelse(y_hat > 5, 5, ifelse(y_hat < 0.5, 0.5, y_hat))

# finally: look at true_y of validation set and compute RMSE
true_y <- fread("movielens_validation.csv") %>% pull(rating)
final_RMSE <- caret::RMSE(y_hat, true_y)
final_RMSE
# Goal: RMSE < 0.8649
# Final RMSE:  0.786


# Progress overview
RMSE_results <- rbind(RMSE_results, c("reco on validation set, clipped", final_RMSE))
RMSE_results %>% mutate(
  RMSE = RMSE %>% as.numeric(),
  Method = Method %>% as_factor() %>% reorder(-as.numeric(.))
  ) %>% 
  ggplot(aes(Method, RMSE)) + geom_point() + theme(axis.text.x = element_text(angle = 90, size = 7)) +
  ggtitle("RMSE of different prediction methods", subtitle = "Validation RSME < 0.79") + 
  coord_flip()

7

