# R code for bikeSharing

# Load random forest library
library(randomForest)

# load helper library
source("helpers.R")

# load training data
bike <- read.csv("data/train.csv")

# Introduce extra columns based on datetime variables
bikeNew <- augmentDateTime(bike)

# Get rid of datetime, registered and casual fields
bikeClean <- bikeNew[c(2:9,12:17)]

bikeTrain <- head(bikeClean,7000)
bikeTestSet <- tail(bikeClean, 3886)

#Train a random forest model
rf <- randomForest(bikeClean$count ~ . , data=bikeClean, ntree=1000)


#calculateRsq(bikeTrain, bikeTestSet, rf)
#RMSLE(bikeTestSet, rf)

# Load the test dataset
bikeTest <- read.csv("data/test.csv")

#Introduce extra columns as done with training set
bikeTestNew <- augmentDateTime(bikeTest)

# Get rid of datetime field
bikeTestClean <- bikeTestNew[c(2:14)]

predictres <- predict(rf, newdata=bikeTestClean)

res["datetime"] <- bikeTest[c(1)]
res["count"] <- predictres

str(res)

write.csv(res, "submission.csv", row.names=FALSE)

