
# recommender package loading

library(recommenderlab)
help(package ="recommenderlab")

set.seed(1)

# datasets 
data_package <- data(package ="recommenderlab")
data_package
data_package$results[,"Item"]
data("MovieLense")

MovieLense

class(MovieLense)

methods(class = class(MovieLense))
methods(class = "realRatingMatrix")

object.size(MovieLense) # realRatingMatrix 

object.size(as(MovieLense,"matrix")) # matrix is size much more than realratingmatrix

object.size(as(MovieLense,"matrix")) / object.size(MovieLense)



similarity_users <- similarity(MovieLense[1:4,], method="cosine", which="users")
class(similarity_users)
as.matrix(similarity_users)

similarity_users1 <- similarity(MovieLense[1:4,], method="pearson", which="users")
class(similarity_users1)
as.matrix(similarity_users1)

similarity_users2 <- similarity(MovieLense[1:4,], method="jaccard", which="users")
class(similarity_users2)
as.matrix(similarity_users2)

image(as.matrix(similarity_users),main="User similarity")

image(as.matrix(similarity_users1),main="User similarity")

image(as.matrix(similarity_users2),main="User similarity")

recommender_models <- recommenderRegistry$get_entries(dataType="realRatingMatrix")

names(recommender_models)

lapply(recommender_models,"[[", "description")
recommender_models


recommender_models$IBCF_realRatingMatrix$parameters

library("ggplot2")
dim(MovieLense)

slotNames(MovieLense)

class(MovieLense@data
)

dim(MovieLense@data)

#exploring the values of the rating 
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

table_ratings <- table(vector_ratings)
table_ratings

vector_ratings <- vector_ratings[vector_ratings != 0]

vector_ratings <- as.factor(vector_ratings)

qplot(vector_ratings) +
  ggtitle("Distribution of the ratings")

views_per_movie <- colCounts(MovieLense)

table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
)

table_views <- table_views[order(table_views$views, decreasing = TRUE),]

ggplot(table_views[1:6,],aes(x=movie, y = views)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1) ) +
  ggtitle("Number of views of the top movies")


# explore the average ratings 

average_ratings <- colMeans(MovieLense)


qplot(average_ratings) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 100 ]

qplot(average_ratings_relevant) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

image(MovieLense,main="Heatmap of the rating matrix")

image(MovieLense[1:10,1:15],main="Heatmap of the rating matrix")

min_n_movies <- quantile(rowCounts(MovieLense),0.99)
min_n_users <- quantile(colCounts(MovieLense),0.99)

min_n_movies

min_n_users

image(MovieLense[rowCounts(MovieLense) > min_n_movies,colCounts(MovieLense) > min_n_users],main="Heatmap of the rating matrix")


rating_movies <- MovieLense[rowCounts(MovieLense) > 50,colCounts(MovieLense) > 100]

min_movies <- quantile(rowCounts(rating_movies),0.98)
min_users <- quantile(colCounts(rating_movies),0.98)

image(MovieLense[rowCounts(MovieLense) > min_movies,colCounts(MovieLense) > min_users],main="Heatmap of the top uses and movies")

average_ratings_per_user <- rowMeans(rating_movies)

qplot(average_ratings_per_user) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

ratings_movies_norm <- normalize(rating_movies)

sum(rowMeans(ratings_movies_norm) > 0.00001)

image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies, 
                          colCounts(ratings_movies_norm) > min_users], 
      main = "Heatmap of the top users and movies")

ratings_movies_watched <- binarize(rating_movies, minRating = 1)

min_movies_binary <- quantile(rowCounts(rating_movies),0.95)
min_users_binary <- quantile(colCounts(rating_movies),0.95)

image(ratings_movies_watched[rowCounts(rating_movies) > min_movies_binary,
                             colCounts(rating_movies) > min_users_binary],
      main="Heatmap of the top users and movies ")


ratings_movies_good <- binarize(rating_movies, minRating = 3)

image(ratings_movies_good[rowCounts(rating_movies) > min_movies_binary,
                          colCounts(rating_movies) > min_users_binary],
      main="Heatmap of the top users and movies ")


# item based collaborative filtering

# training and test set 

which_train <- sample(x = c(TRUE, FALSE), size = nrow(rating_movies),replace = TRUE, prob=c(0.8,0.2))
recc_data_train <- rating_movies[which_train,]

recc_dataa_test <- rating_movies[!which_train,]

which_set <- sample(x = 1:5, size =nrow(rating_movies), 
                    replace= TRUE )

for (i_model in 1:5 ){
  which_train <- which_set == i_model 
  recc_data_train <- rating_movies[which_train,]
  recc_data_test <- rating_movies[!which_train,]
}


# building the recommendation model 

recc_model <- Recommender(data = recc_data_train,method="IBCF", parameter = list(k = 30))

class(recc_model)


# exploring the recommended model 

model_details <- getModel(recc_model)
model_details$description

model_details$sim

class(model_details$sim)

dim(model_details$sim)

n_items_top <- 20

image(model_details$sim[1:n_items_top, 1:n_items_top], main = "Heatmap of the first rows and columns" )

model_details$k

row_sums <- rowSums(model_details$sim > 0)

table(row_sums)

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1 ) +
  ggtitle("Distribution of the column count")

which_max <- order(col_sums, decreasing = TRUE)[1:6]

rownames(model_details$sim)[which_max]


n_recommend <- 6

recc_predicted <- predict(object= recc_model, newdata = recc_data_test, n= n_recommend)

class(recc_predicted)


recc_user_1 <- recc_predicted@items[[1]]

movies_user_1 <- recc_predicted@itemLabels[recc_user_1]

movies_user_1

recc_matrix <- sapply(recc_predicted@items,
                      function(x) {
                        colnames(rating_movies) [x]
                      })

dim(recc_matrix)

recc_matrix[,1:4]

number_of_items <- factor(table(recc_matrix))
chart_title <-  "Distribution of the number of items for IBCF"

qplot(number_of_items) +
  ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing =  TRUE)
number_of_items_top <- head(number_of_items_sorted, n= 4)
table_top <- data.frame(names(number_of_items_top),number_of_items_top)
table_top


# building the UBCF recommendation model 

ubcf_recc_model <- Recommender(data = recc_data_train,method="IBCF", parameter = list(k = 30))

class(ubcf_recc_model)


# exploring the ubcf recommended model 

ubcf_model_details <- getModel(ubcf_recc_model)
ubcf_model_details$description

ubcf_model_details$sim

class(ubcf_model_details$sim)

dim(ubcf_model_details$sim)

n_items_top <- 20

image(ubcf_model_details$sim[1:n_items_top, 1:n_items_top], main = "Heatmap of the first rows and columns" )

model_details$k

row_sums <- rowSums(ubcf_model_details$sim > 0)

table(row_sums)

col_sums <- colSums(ubcf_model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1 ) +
  ggtitle("Distribution of the column count")

which_max <- order(col_sums, decreasing = TRUE)[1:6]

rownames(ubcf_model_details$sim)[which_max]


n_recommend <- 6

ubcf_recc_predicted <- predict(object= ubcf_recc_model, newdata = recc_data_test, n= n_recommend)

class(ubcf_recc_predicted)


recc_user_1 <- ubcf_recc_predicted@items[[1]]

movies_user_1 <- ubcf_recc_predicted@itemLabels[recc_user_1]

movies_user_1

ubcf_recc_matrix <- sapply(ubcf_recc_predicted@items,
                      function(x) {
                        colnames(rating_movies) [x]
                      })

dim(ubcf_recc_matrix)

ubcf_recc_matrix[,1:4]

ubcf_number_of_items <- factor(table(ubcf_recc_matrix))
ubcf_chart_title <-  "Distribution of the number of items for IBCF"

qplot(ubcf_number_of_items) +
  ggtitle(ubcf_chart_title)

ubcf_number_of_items_sorted <- sort(ubcf_number_of_items, decreasing =  TRUE)
ubcf_number_of_items_top <- head(ubcf_number_of_items_sorted, n= 4)
ubcf_table_top <- data.frame(names(ubcf_number_of_items_top),ubcf_number_of_items_top)
ubcf_table_top

rating_movies <- MovieLense[rowCounts(MovieLense) > 50,colCounts(MovieLense) > 100]

ratings_movies_watched <- binarize(rating_movies, minRating = 1)

qplot(rowSums(ratings_movies_watched)) + stat_bin(binwidth = 10) + 
  geom_vline(xintercept = mean(rowSums(ratings_movies_watched)), col = "red", 
             linetype = "dashed")
+ ggtitle("Distribution of movies by user")

# evaluation

percentage_training <- 0.8

min(rowCounts(rating_movies))


items_to_keep <- 15

# how many times run the evaluation

n_eval <- 1

## Evaluation scheme with 15 items given
## Method: 'split' with 1 run(s).
## Training set proportion: 0.800
## Good ratings: >=3.000000
## Data set: 560 x 332 rating matrix of class 'realRatingMatrix' with 55298 ratings.

rating_threshold <- 3.0

eval_sets <- evaluationScheme(data = rating_movies, method = "split", train = percentage_training, given = items_to_keep, goodRating = rating_threshold, k = n_eval) 
eval_sets

getData(eval_sets, "train")

getData(eval_sets, "known")

getData(eval_sets,"unknown")

nrow(getData(eval_sets, "train")) / nrow(rating_movies)

nrow(getData(eval_sets, "known")) / nrow(rating_movies)

unique(rowCounts(getData(eval_sets, "known")))


qplot(rowCounts(getData(eval_sets, "unknown"))) 
+ geom_histogram(binwidth = 10) 
+ ggtitle("unknown items by the users")


percentage_training <- 0.8
items_to_keep <- 15
rating_threshold <- 3
n_eval <- 1

eval_sets <- evaluationScheme(data = rating_movies, method = "bootstrap", train = percentage_training, given = items_to_keep, goodRating = rating_threshold, k = n_eval)

nrow(getData(eval_sets, "train")) / nrow(rating_movies)## _0.8_

perc_test <- nrow(getData(eval_sets, "known")) / nrow(rating_movies)

perc_test

length(unique(eval_sets@runsTrain[[1]]))

perc_train <- length(unique(eval_sets@runsTrain[[1]])) / nrow(rating_movies)

perc_train + perc_test## _1_

table_train <- table(eval_sets@runsTrain[[1]])
n_repetitions <- factor(as.vector(table_train))

qplot(n_repetitions) 
+ ggtitle("Number of repetitions in the training set")


n_fold <- 4
eval_sets <- evaluationScheme(data = rating_movies, method = "cross-validation", k = n_fold, given = items_to_keep, goodRating = rating_threshold)

size_sets <- sapply(eval_sets@runsTrain, length)
size_sets
## _420_, _420_, _420_ and _420_


# evaluating the ratings

n_fold <- 4
items_to_keep <- 15
rating_threshold <- 3
eval_sets <- evaluationScheme(data = rating_movies, method = "cross-validation", k = n_fold, given = items_to_keep, goodRating = rating_threshold)


model_to_evaluate <- "IBCF"
model_parameters <- NULL

eval_recommender <- Recommender(data = getData(eval_sets, "train"), method = model_to_evaluate, parameter = model_parameters)

eval_prediction <- predict(object = eval_recommender, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings") 

class(eval_prediction)## realRatingMatrix

items_to_recommend <- 10
# no of movies per user
qplot(rowCounts(eval_prediction)) +
  geom_histogram(binwidth = 10) + 
  ggtitle("Distribution of movies per user")


#measure the accuracy of the model 

eval_accuracy <- calcPredictionAccuracy(  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = TRUE)

head(eval_accuracy)


# RMSE by User

qplot(eval_accuracy[, "RMSE"]) 
+ geom_histogram(binwidth = 0.1) 
+ ggtitle("Distribution of the RMSE by user")


#performance of the model 

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = FALSE) 

eval_accuracy


# evaluating the model

results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10, 100, 10))

class(results)


head(getConfusionMatrix(results)[[1]])



columns_to_sum <- c("TP", "FP", "FN", "TN")

indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)


plot(results, annotate = TRUE, main = "ROC curve")

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")


# comparing  models


models_to_evaluate <- list(  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
                             IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
                             UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
                             UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
                             random = list(name = "RANDOM", param=NULL))

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)
class(list_results)## evaluationResultList

class(list_results[[1]])

sapply(list_results, class) == "evaluationResults"


avg_matrices <- lapply(list_results, avg)


head(avg_matrices$IBCF_cos[, 5:8])


plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 

title("Precision-recall")


# optimize model parameter

vector_k <- c(5, 10, 20, 30, 40)


#For instance, IBCF takes account of the k-closest items

models_to_evaluate <- lapply(vector_k, function(k)
{  list(name = "IBCF", param = list(method = "cosine", k = k))})
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)


plot(list_results, annotate = 1,legend = "topleft") 

title("ROC curve")


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")

title("Precision-recall")
