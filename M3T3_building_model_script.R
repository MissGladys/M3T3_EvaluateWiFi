library(tidyverse)
library(lattice)
library(caret)

source("C:/Users/Splendora/OneDrive/Ubiqum/Module_3_Task_3_EvaluateWiFi/M3T3_EvaluateWiFi/M3T3_preprocess_script.R")


# Create 20% Sample Set from training_orig data and then split to train and test sets
set.seed(745)
trainSize_sample <- round(nrow(training_orig)*0.20)
testSize_orig <- nrow(training_orig) - trainSize_sample

training_indices <- sample(seq_len(nrow(training_orig)), size = trainSize_sample)
trainSet_sample <- training_orig[training_indices,]
testSet_sample <- training_orig[-training_indices,]

# Convert data types
# trainSet_sample$Longitude <- as.factor(trainSet_sample$Longitude)
# trainSet_sample$Latitude <- as.factor(trainSet_sample$Latitude)
# trainSet_sample$Floor <- as.factor(trainSet_sample$Floor)
# trainSet_sample$Building_ID <- as.numeric(trainSet_sample$Building_ID)
# trainSet_sample$Space_ID <- as.factor(trainSet_sample$Space_ID)
# trainSet_sample$Relative_Position <- as.factor(trainSet_sample$Relative_Position)
# trainSet_sample$User_ID <- as.factor(trainSet_sample$User_ID)
# trainSet_sample$Phone_ID <- as.factor(trainSet_sample$Phone_ID)
# trainSet_sample$Timestamp <- as.factor(trainSet_sample$Timestamp)
# 
# testSet_sample$Longitude <- as.factor(testSet_sample$Longitude)
# testSet_sample$Latitude <- as.factor(testSet_sample$Latitude)
# testSet_sample$Floor <- as.factor(testSet_sample$Floor)
# testSet_sample$Building_ID <- as.numeric(testSet_sample$Building_ID)
# testSet_sample$Space_ID <- as.factor(testSet_sample$Space_ID)
# testSet_sample$Relative_Position <- as.factor(testSet_sample$Relative_Position)
# testSet_sample$User_ID <- as.factor(testSet_sample$User_ID)
# testSet_sample$Phone_ID <- as.factor(testSet_sample$Phone_ID)
# testSet_sample$Timestamp <- as.factor(testSet_sample$Timestamp)


# Separate data by building from trainSet_sample
trainSetsample_building0 <- trainSet_sample %>%
  filter(Building_ID == 0)
trainSetsample_building1 <- trainSet_sample %>%
  filter(Building_ID == 1)
trainSetsample_building2 <- trainSet_sample %>%
  filter(Building_ID == 2)


# Create a data frame for each feature from trainSet_sample
## Building0
trainSetsample_building0_floor <- data.frame(trainSetsample_building0$Floor,
                                             trainSetsample_building0 %>%
                                               select(starts_with("WAP")))

trainSetsample_building0_latitude <- data.frame(trainSetsample_building0$Latitude,
                                                trainSetsample_building0 %>%
                                                  select(starts_with("WAP")))

trainSetsample_building0_longitude <- data.frame(trainSetsample_building0$Longitude,
                                                 trainSetsample_building0 %>%
                                                   select(starts_with("WAP")))

## Building1
trainSetsample_building1_floor <- data.frame(trainSetsample_building1$Floor,
                                             trainSetsample_building1 %>%
                                               select(starts_with("WAP")))

trainSetsample_building1_latitude <- data.frame(trainSetsample_building1$Latitude,
                                                trainSetsample_building1 %>%
                                                  select(starts_with("WAP")))

trainSetsample_building1_longitude <- data.frame(trainSetsample_building1$Longitude,
                                                 trainSetsample_building1 %>%
                                                   select(starts_with("WAP")))

## Building2
trainSetsample_building2_floor <- data.frame(trainSetsample_building2$Floor,
                                             trainSetsample_building2 %>%
                                               select(starts_with("WAP")))
trainSetsample_building2_latitude <- data.frame(trainSetsample_building2$Latitude,
                                                trainSetsample_building2 %>%
                                                  select(starts_with("WAP")))
trainSetsample_building2_longitude <- data.frame(trainSetsample_building2$Longitude,
                                                 trainSetsample_building2 %>%
                                                   select(starts_with("WAP")))


# Training building0 - method: knn

## applying 10-fold cross validation
set.seed(745)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

knnTrain_building0 <- train(Floor~.,
                        data = trainSetsample_building0,
                        method = "knn",
                        trControl = fitControl,
                        tuneLength = 2,
                        preProcess=c("center","scale"))
## result of training
knnTrain_building0

## variable importance
varImp(knnTrain_building0)

# ggplot(knnTrain_building0)

## Predicting the test set
knnPredict_building0 <- predict(knnTrain_building0, testSet_sample)
knnPredict_building0

## Confusion Matrix
confusionMatrix(knnPredict_building0,testSet_sample$Building_ID)

str(knnPredict_building0)
str(testSet_sample$Building_ID)



# =============
# #training building0 - method: random forest
# rfTrain_building0 <- train(Floor~.,
#                            data = sample_building0,
#                            method = "rf",
#                            trControl = fitControl,
#                            preProcess=c("center","scale"))
# rfTrain_building0
# ggplot(rfTrain_building0)
# 
# #training building0 - method: svm linear
# svmTrain_building0 <- train(Floor~.,
#                            data = sample_building0,
#                            method = "svmLinear",
#                            trControl = fitControl,
#                            tuneLength = 2,
#                            preProcess=c("center","scale"))
