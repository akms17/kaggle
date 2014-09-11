# R code for bikeSharing

# Load random forest library
library(randomForest)

# load helper library
source("helpers.R")

# load training data
bike <- read.csv("data/train.csv")

# Introduce extra columns based on datetime variables
bikeAugment <- augmentDateTime(bike)

# Get rid of datetime, registered and casual fields
bikeClean <- bikeAugment[c(2:9,12:17)]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(bikeClean))

## set the seed to make your partition reproductible
set.seed(312)
train_ind <- sample(seq_len(nrow(bikeClean)), size = smp_size)

bikeTrain <- bikeClean[train_ind, ]
bikeTest <- bikeClean[-train_ind, ]


#Find the optimal number of variables to try and split on each node
#bestmtry <- tuneRF(bikeTrain[-9],bikeTrain$count, ntreeTry=100, stepfactor=1.5, improve=0.02, trace=TRUE, plot=TRUE, dobest=FALSE)

# This states that mtry 13 was the optimal number of variables to consider.

#Train a random forest model
rf <- randomForest(count ~ . , data=bikeTrain, mtry=13, ntree=1000, keep.FOREST=TRUE, importance=TRUE, test=bikeTest)

#calculateRsq(bikeTrain, bikeTestSet, rf)
RMSLE(bikeTest, rf)

# Load the test dataset
bikeNew <- read.csv("data/test.csv")

#Introduce extra columns as done with training set
bikeNewAugment <- augmentDateTime(bikeNew)

# Get rid of datetime field
bikeNewClean <- bikeNewAugment[c(2:14)]

predictres <- predict(rf, newdata=bikeNewClean)

res <- data.frame(datetime = bikeNew[c(1)], count = predictres)
str(res)

write.csv(res, "submission.csv", row.names=FALSE)

