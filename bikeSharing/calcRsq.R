#R Sample code
calculateRsq <- function(train, test, model) {
	prediction = predict(model, newdata=test)
	SSE = sum((test$count - prediction)^2)
	SST = sum((test$count - mean(train$count))^2)
	1 - SSE/SST  
}
