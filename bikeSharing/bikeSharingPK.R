# R code for bikeSharing

# Load random forest library
library(randomForest)
library(ggplot2)

# load helper library
source("helpers.R")

# load training data
bike <- read.csv("data/train.csv")

# Introduce extra columns based on datetime variables
bikeAugment <- augmentDateTime(bike)

# Get rid of datetime, keep (registered and casual fields)  for now also convert columns to factor ones.
bikeClean <- bikeAugment[c(2:16)]

# Add a special field for Rains
#bikeWithRains <- addRains(bikeLess)

# Add a special field for spring (day light saving is a bad name)
#bikeClean <- addDayLightSaving(bikeWithRains)

bikeCasualT <- augmentForCasual(bikeClean)
bikeCasualA <- bikeCasualT[c(1:9,12:17)]
bikeCasual <- augmentDaypart(bikeCasualA)


# 
## 75% of the sample size
smp_size <- floor(0.75 * nrow(bikeCasual))

## set the seed to make your partition reproductible
set.seed(312)
train_ind <- sample(seq_len(nrow(bikeCasual)), size = smp_size)

bikeCasualTrain <- bikeCasual[train_ind, ]
bikeCasualTest <- bikeCasual[-train_ind, ]

#Train a random forest model
cas_rf <- randomForest(casual ~ season + holiday + workingday + weather + atemp + humidity + windspeed + weekday + year + month + hour + winter + rain + daypart, data=bikeCasualTrain, mtry=13, ntree=1000, keep.FOREST=TRUE, importance=TRUE, test=bikeCasualTest)
varImpPlot(cas_rf)

#RMSLE(bikeTest, cas_rf)

bikeRegT <- augmentForRegistered(bikeClean)
bikeRegA <- bikeRegT[c(1:7,9,11:16)]
bikeReg <- augmentDaypart(bikeRegA)

# 
## 75% of the sample size
smp_size <- floor(0.75 * nrow(bikeReg))

## set the seed to make your partition reproductible
set.seed(312)
train_ind <- sample(seq_len(nrow(bikeReg)), size = smp_size)

bikeRegTrain <- bikeReg[train_ind, ]
bikeRegTest <- bikeReg[-train_ind, ]

#Train a random forest model
reg_rf <- randomForest(registered ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + weekday + year + month + hour + spring + rain + daypart, data=bikeRegTrain, mtry=13, ntree=1000, keep.FOREST=TRUE, importance=TRUE, test=bikeRegTest)
varImpPlot(reg_rf)

#RMSLE(bikeTest, reg_rf)

cas_pred <- predict(cas_rf, newdata=bikeCasualTest)

reg_pred <- predict(reg_rf, newdata=bikeRegTest)

count_pred <- cas_pred + reg_pred

counterror(bikeRegTest, count_pred)

# Load the test dataset
bikeNew <- read.csv("data/test.csv")

#Introduce extra columns as done with training set
bikeNewAugment <- augmentDateTime(bikeNew)

# Get rid of datetime field
bikeNewClean <- bikeNewAugment[c(2:5,7:13)]

bikeNewX <- augmentForCasual(bikeNewClean)
bikeNewY <- augmentForRegistered(bikeNewX)

caspred <- predict(cas_rf, newdata=bikeNewY)
regpred <- predict(reg_rf, newdata=bikeNewY)

predictres <- caspred + regpred

res <- data.frame(datetime = bikeNew[c(1)], count = predictres)
str(res)

write.csv(res, "submission.csv", row.names=FALSE)
