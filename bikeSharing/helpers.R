#R helper methods

calculateRsq <- function(train, test, model) {
	prediction = predict(model, newdata=test)
	SSE = sum((test$count - prediction)^2)
	SST = sum((test$count - mean(train$count))^2)
	1 - SSE/SST  
}

augmentDateTime <- function(dataset) {
    dt <-  as.POSIXlt(dataset$datetime, "%Y-%m-%d %H:%M:%S")
    dataset["weekday"]      <- as.factor(dt$wday)
    dataset["year"]         <- as.factor(dt$year + 1900)
    dataset["month"]        <- as.factor(dt$mon)
    dataset["hour"]         <- as.numeric(dt$hour)
    dataset$weather         <- as.factor(dataset$weather)
    dataset$holiday         <- as.factor(dataset$holiday)
    dataset$workingday      <- as.factor(dataset$workingday)
    dataset$season          <- as.factor(dataset$season)
    return(dataset)
}

RMSLE <- function(test, model) {
    prediction <- predict(model, newdata=test)
    res <- sqrt((1/nrow(test) * sum((log(prediction + 1) - log(test$count + 1))^2)))
    res
}


RMSLEcasual <- function(test, model) {
    prediction <- predict(model, newdata=test)
    res <- sqrt((1/nrow(test) * sum((log(prediction + 1) - log(test$casual + 1))^2)))
    res
}

RMSLEreg <- function(test, model) {
    prediction <- predict(model, newdata=test)
    res <- sqrt((1/nrow(test) * sum((log(prediction + 1) - log(test$registered + 1))^2)))
    res
}


#addFeatureFromColumn <- function(dataset, featureMap, featurename, FUN) {
 # dataset[featurename] <- 0  
  
  #for (i in 0:nrow(hc)-1){
  #  bikeClean$hrmediancnt[bikeClean$hour == i] <- as.numeric(hc[hc$Group.1 == i,][[2]])
  #}
#}


addRains <- function(dataset) {
  dataset$rain[dataset$weather == 4] <- 1
  dataset$rain[dataset$weather != 4] <- 0
  dataset$rain <- as.factor(dataset$rain)
  return (dataset)
}

addDayLightSaving <- function(dataset) {
  dataset$spring[dataset$season == 1] <- 1
  dataset$spring[dataset$season != 1] <- 0
  dataset$spring <- as.factor(dataset$spring)
  return (dataset)
}

augmentForCasual <- function(dataset) {
  dataset$winter[dataset$season == 1 | dataset$season == 4] <- 0
  dataset$winter[dataset$season == 2 | dataset$season == 3] <- 1
  dataset$winter <- as.factor(dataset$winter)

  dataset$rain[dataset$weather == 4 | dataset$weather == 3] <- 0
  dataset$rain[dataset$weather == 2 | dataset$weather == 1] <- 1
  dataset$rain <- as.factor(dataset$rain)
  return (dataset)
}

augmentForRegistered <- function(dataset) {
  dataset$spring[dataset$season == 1] <- 0
  dataset$spring[dataset$season != 1] <- 1
  dataset$spring <- as.factor(dataset$spring)

  dataset$rain[dataset$weather == 3 | dataset$weather == 4] <- 0
  dataset$rain[dataset$weather == 2 | dataset$weather == 1] <- 1
  dataset$rain <- as.factor(dataset$rain)
  
  return (dataset)
}

augmentDaypart <- function(dataset) {
  #create daypart column, default to 4 to make things easier for ourselves
  dataset$daypart <- "4"
  
  #4AM - 10AM = 1
  dataset$daypart[(dataset$hour < 10) & (dataset$hour > 3)] <- 1
  
  #11AM - 3PM = 2
  dataset$daypart[(dataset$hour < 16) & (dataset$hour > 9)] <- 2

  #4PM - 9PM = 3
  dataset$daypart[(dataset$hour < 22) & (dataset$hour > 15)] <- 3

  #convert daypart to factor
  dataset$daypart <- as.factor(dataset$daypart)

  #convert hour back to factor
  dataset$hour <- as.factor(dataset$hour)
  return (dataset)
}
