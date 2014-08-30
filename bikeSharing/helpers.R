#R helper methods

calculateRsq <- function(train, test, model) {
	prediction = predict(model, newdata=test)
	SSE = sum((test$count - prediction)^2)
	SST = sum((test$count - mean(train$count))^2)
	1 - SSE/SST  
}

augmentDateTime <- function(dataset) {
    dt <-  as.POSIXlt(dataset$datetime, "%Y-%m-%d %H:%M:%S")
    dataset["weekday"]      <- dt$wday
    dataset["year"]         <- dt$year + 1900
    dataset["month"]        <- dt$mon
    dataset["hour"]         <- dt$hour
    dataset["dayOfMonth"]   <- dt$mday    
    return(dataset)
}

RMSLE <- function(test, model) {
    prediction <- predict(model, newdata=test)
    res <- sqrt((1/nrow(test) * sum((log(prediction + 1) - log(test$count + 1))^2)))
    res
}
