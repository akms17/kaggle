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

# Based on prior knowledge we know hour is most influential factor, so we add mean count per hour as a feature
#hc <- aggregate(bikeClean$count, by=list(bikeClean$hour), FUN=median)

#bikeClean["hrmediancnt"] <- 0

#`for (i in 0:nrow(hc)-1){
#  bikeClean$hrmediancnt[bikeClean$hour == i] <- as.numeric(hc[hc$Group.1 == i,][[2]])
#}

## 75% of the sample size
smp_size <- floor(0.75 * nrow(bikeClean))

## set the seed to make your partition reproductible
set.seed(312)
train_ind <- sample(seq_len(nrow(bikeClean)), size = smp_size)

bikeTrain <- bikeClean[train_ind, ]
bikeTest <- bikeClean[-train_ind, ]


# Get rid of some useless features like season weekday


#Find the optimal number of variables to try and split on each node for prediciting registered
reg_mtry <- tuneRF(bikeTrain[-c(9:11)], bikeTrain$registered, ntreeTry=100, stepfactor=1.5, improve=0.01, trace=TRUE, plot=FALSE, dobest=FALSE)
minindex <- which.min(reg_mtry[,2])

#Train a random forest model
reg_rf <- randomForest(registered ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + weekday + year + month + hour , data=bikeTrain, mtry=reg_mtry[minindex], ntree=1000, keep.FOREST=TRUE, importance=TRUE, test=bikeTest)
varImpPlot(reg_rf)

RMSLE(bikeTest, reg_rf)

# Now predict casual
# Find the optimal number of variables to try and split on each node for prediciting casual
bestmtry <- tuneRF(bikeTrain[-c(9:11)],bikeTrain$casual, ntreeTry=100, stepfactor=1.5, improve=0.01, trace=TRUE, plot=FALSE, dobest=FALSE)
minidx <- which.min(bestmtry[,2])

cas_rf <- randomForest(casual ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + weekday + year + month + hour , data=bikeTrain, mtry=bestmtry[minidx], ntree=1000, keep.FOREST=TRUE, importance=TRUE, test=bikeTest)
varImpPlot(cas_rf)

RMSLE(bikeTest, cas_rf)

# Now predicting count
# Find the optimal number of variables to try and split on each node for prediciting count
newTrain <- bikeTrain[-c(9:10)]
newTrain["reg_p"] <- predict(reg_rf, newdata=bikeTrain)
newTrain["cas_p"] <- predict(cas_rf, newdata=bikeTrain)

newTest <- bikeTest[-c(9:10)]
newTest["reg_p"] <- predict(reg_rf, newdata=bikeTest) 
newTest["cas_p"] <- predict(cas_rf, newdata=bikeTest) 

mtry <- tuneRF(newTrain[-9], newTrain$count, ntreeTry=100, stepfactor=1.5, improve=0.01, trace=TRUE, plot=FALSE, dobest=FALSE)
minidx <- which.min(mtry[,2])
rf <- randomForest(count ~ . , data=newTrain, mtry=mtry[minidx], ntree=5000, keep.FOREST=TRUE, importance=TRUE, test=newTest)
varImpPlot(rf)

calculateRsq(newTrain, newTest, rf)
RMSLE(newTest, rf)

# Load the test dataset
bikeNew <- read.csv("data/test.csv")

#Introduce extra columns as done with training set
bikeNewAugment <- augmentDateTime(bikeNew)

# Get rid of datetime field
bikeNewClean <- bikeNewAugment[c(2:13)]

#bikeNewClean["hrmediancnt"] <- 0

#for (i in 0:nrow(hc)-1){
#  bikeNewClean$hrmediancnt[bikeNewClean$hour == i] <- as.numeric(hc[hc$Group.1 == i,][[2]])
#}

bikeNewClean["reg_p"] <- predict(reg_rf, newdata=bikeNewClean)
bikeNewClean["cas_p"] <- predict(cas_rf, newdata=bikeNewClean)

predictres <- predict(rf, newdata=bikeNewClean)

res <- data.frame(datetime = bikeNew[c(1)], count = predictres)
str(res)

write.csv(res, "submission.csv", row.names=FALSE)

