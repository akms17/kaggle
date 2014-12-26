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
    dataset["hour"]         <- as.factor(dt$hour)
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


#addFeatureFromColumn <- function(dataset, featureMap, featurename, FUN) {
 # dataset[featurename] <- 0  
  
  #for (i in 0:nrow(hc)-1){
  #  bikeClean$hrmediancnt[bikeClean$hour == i] <- as.numeric(hc[hc$Group.1 == i,][[2]])
  #}
#}
