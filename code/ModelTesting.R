
# Clean and set up environment --------------------------------------------

rm(list = ls())

# setwd("Z:/Cristian Acuna/Module 2/Research Project/Toy to Enjoy/code")

library(magrittr)
library(readr)
library(tm)
library(RColorBrewer)
library(caret)
library(e1071)
library(ranger)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tictoc)
#library(kknn)

# Load data ---------------------------------------------------------------

reviews <- read_csv("../input/scrapped_reviews.csv")
reviews <- reviews[, -1]
reviews <- reviews[!(is.na(reviews$Review) | reviews$Review == ""), ]
reviews$Review %<>%  iconv("ASCII//TRANSLIT")
reviews$Review %<>% tolower()
reviews[reviews$Review %in% c("100%", "100 %"), "Review"] <- "excellent"

# Subseting dataset -------------------------------------------------------

fiveStars	<- reviews[reviews$Rating == 5, ]
fourStars 	<- reviews[reviews$Rating == 4, ]
threeStars 	<- reviews[reviews$Rating == 3, ]
twoStars 	<- reviews[reviews$Rating == 2, ]
oneStar  	<- reviews[reviews$Rating == 1, ]

fiveStars 	%<>%  sample_n(size = 200)
fourStars 	%<>%  sample_n(size = 200)
threeStars 	%<>%  sample_n(size = 200)
twoStars 	%<>%  sample_n(size = 200)
oneStar 	%<>%  sample_n(size = 200)

reviews <- rbind(fiveStars, fourStars, threeStars, twoStars, oneStar)

# Building the corpus -----------------------------------------------------

reviews_comments <- reviews$Review
reviews_com_source <- VectorSource(reviews_comments)
reviews_com_corpus <- VCorpus(reviews_com_source)
#str(reviews_com_corpus)

clean_corpus <- function(corpus){
	corpus <- tm_map(corpus, removePunctuation)
	corpus <- tm_map(corpus, removeNumbers)
	corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "Patatas"))
	corpus <- tm_map(corpus, stripWhitespace)
	return(corpus)
}

clean_reviews <- clean_corpus(reviews_com_corpus)

# Building DTM ------------------------------------------------------------

reviews_dtm <-
	DocumentTermMatrix(
		clean_reviews,
		list(
			removePunctuation = TRUE,
			removeNumbers = TRUE,
			stopwords = TRUE,
			weighting = weightTfIdf))

reviews_matrix <- as.matrix(reviews_dtm)
reviews_dt <- as.data.frame(reviews_matrix)

dataset <- cbind(reviews_dt, "Score" = as.factor(reviews$Rating))

# building the test and training sets -------------------------------------

train_index <- createDataPartition(y = dataset$Score, p = 0.75, list = FALSE)
train <- dataset[train_index, ]
test <- dataset[-train_index, ]

# training the models -----------------------------------------------------

#first_train_SVM <- train(Score~.,data = train, method = "svmLinear2")
#first_predict_SVM <- predict(first_train_SVM,test)
#confusionMatrix(first_predict_SVM,test$Score)

#first_train_RandForest <- train(Score~.,data = train, method = "ranger")
#first_predict_randForest <- predict(first_train_RandForest,test)
#confusionMatrix(first_predict_randForest ,test$Score)

#svm_model <- svm(reviews_dt, train$Score, cost = 100, gamma = 1)

# svm linear --------------------------------------------------------------

tic("SVM Linear")

svm_model <- svm(train[, 1:(ncol(train) - 1)], train$Score, cost = 100, kernel = "linear")
svm_pred <- predict(svm_model, test[,-ncol(test)])
confusionMatrix(svm_pred, test$Score)

toc()
# tgrid <- expand.grid(
# 	.num.trees = 50
# )

# ranger num.trees=50 -----------------------------------------------------

tic("RandomForest with num.trees=50")

ranger_model <- train(x = train[, -ncol(train)],
					  y = train$Score,
					  method = "ranger",
					  num.trees = 50)
ranger_pred <- predict(ranger_model, test)
confusionMatrix(ranger_pred ,test$Score)

toc()

# svm rbf -----------------------------------------------------------------

tic("SVM Radial Basis Function")

svm_rbf_model <- svm(train[, 1:(ncol(train) - 1)], train$Score, cost = 100, gamma = 1)
svm_rbf_pred <- predict(svm_rbf_model, test[,-ncol(test)])
confusionMatrix(svm_rbf_pred, test$Score)

toc()

# kNN ---------------------------------------------------------------------

tic("kNN")

knn_model <- train(x = train[, -ncol(train)],
				   y = train$Score,
				   method = "knn")
knn_pred <- predict(knn_model, test[,-ncol(test)])
confusionMatrix(knn_pred, test$Score)

toc()
