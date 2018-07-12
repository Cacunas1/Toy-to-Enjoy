# cleaning and importing the data -----------------------------------------
rm(list = ls())

library(readr)
library(tibble)
library(datasets)
library(magrittr)
library(tm)
library(caret)

# Reading the data --------------------------------------------------------
reviewTable <- read_tsv("../input/reviews.txt")
View(reviewTable)

# Preparing the text ------------------------------------------------------
data <- reviewTable$Review

corpus <- VCorpus(VectorSource(data))
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
train <- as.matrix(tdm)

train <- cbind(reviewTable$Score)
colnames(train)[ncol(train)] <- 'y'
colnames(train)[1:ncol(train)-1] <- 'x'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Training the model ------------------------------------------------------
fit <- train(y ~ ., data = train, method = 'bayesglm')

Check accuracy on training.
predict(fit, newdata = train)