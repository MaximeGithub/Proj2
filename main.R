if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
if("TTR" %in% rownames(installed.packages()) == FALSE) {install.packages("TTR")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
if("elasticnet" %in% rownames(installed.packages()) == FALSE) {install.packages("elasticnet")}
if("glmnet" %in% rownames(installed.packages()) == FALSE) {install.packages("glmnet")}
if("rhdf5" %in% rownames(installed.packages()) == FALSE) {
	source("http://bioconductor.org/biocLite.R")
	biocLite("rhdf5")
}

require(data.table)
require(TTR)
require(caret)
require(plyr)
require(MASS)
require(elasticnet)
require(glmnet)
require(rhdf5)

path = "D:\\Programmation\\R\\Eclipse\\Test\\XTX\\"
setwd(path)

source(paste(path,"data_functions.R",sep=""))
source(paste(path,"signals_functions.R",sep=""))
source(paste(path,"util.R",sep=""))

############ load data #################
cat("\n\n#############   Loading data ###################")
data    = loadData("data.mat")
askRate = data[["askRate"]]
askSize = data[["askSize"]]
bidRate = data[["bidRate"]]
bidSize = data[["bidSize"]]
y       = data[["y"]]
names(y) = c("y")
data = mergeOBintoSingleDf(askRate, askSize, bidRate, bidSize)
rm(list=c("askRate", "bidRate", "askSize", "bidSize"))
############ parameters ################
#rangeAmounts = c(10, 50)
#listEwma = c(1,10,50,100)
normalizationType = "standard"  #"standard" or "rolling"
rangeAmounts = c(5, 20, 50, 100)
listLags = c(1, 20, 50, 100)
listEwma = c(0, 10, 30)
rangeAmounts = c(5, 10, 15, 20, 30, 40, 50, 60, 70)
listLags = c(1, 10, 200)
listEwma = c(0, 10, 50, 100, 200, 500, 1000, 2000)
#rangeAmounts = c(5, 10, 20)
#listLags = c(1, 10, 30)
#listEwma = c(0, 10, 30)
numBucketsVolatility = 1
alphaElasticNet = 0.9 # 1 is lasso

########### create validation and test sets ##############
folds <- cut(seq(1,nrow(data)),breaks=20,labels=FALSE)
capAndLimitFolds = 1:2
crossValidationFolds = 3:12
#crossValidationFolds = 2:9 ################################################################ testing purpose only
falseTestFolds = 13:16
testingFolds = 17:20

dataLimits = data[folds %in% capAndLimitFolds]
yLimits = y[folds %in% capAndLimitFolds]

dataCV = data[folds %in% crossValidationFolds]
yCV = y[folds %in% crossValidationFolds]

dataTest = data[folds %in% falseTestFolds]
yTest = y[folds %in% falseTestFolds]

realDataTest = data[folds %in% testingFolds]
realYTest = y[folds %in% testingFolds]


liquidityStrength = function(df_askRate, df_askSize, df_bidRate, df_bidSize){
	askStrength = ((df_askRate * 10) %% 10) != 5 
	bidStrength = ((df_bidRate * 10) %% 10) != 5
	return(data.table(levelStrength = askStrength-bidStrength))
}
#data = cbind(dataLimits, yLimits)
#
#data[,topAsk:=topAsk]
#topBid = (data$bid_price_1 * 10) %% 10
#data[,topBid:=topBid]
############# compute limits and signals ####################
cat("\n\n#############   Calibrating the limits ###################\n")
capLimits = calibrateCapAndFloorsForAllSignals(dataLimits, rangeAmounts, listLags, listEwma, numBucketsVolatility, normalizationType)
cat("\n\n#############   Computing signals for cross-validation ###################\n")
signals = computeSignalsAndApplyLimits(dataCV, rangeAmounts, listLags, listEwma, capLimits, normalizationType)


#signals[,test1:=ifelse(diff_weighted_5_ewma_0*arrivalDeparture.arrival_departure_level1_ewma_10>0,sign(diff_weighted_5_ewma_0) * diff_weighted_5_ewma_0*arrivalDeparture.arrival_departure_level1_ewma_10,0)]
#signals[,test2:=ifelse(diff_weighted_5_ewma_0*diff_weighted_20_ewma_0>0,sign(diff_weighted_5_ewma_0) * diff_weighted_5_ewma_0*diff_weighted_20_ewma_0,0)]
#signals[,test3:=ifelse(diff_weighted_5_ewma_0*diff_weighted_40_ewma_0>0,sign(diff_weighted_5_ewma_0) * diff_weighted_5_ewma_0*diff_weighted_40_ewma_0,0)]
############ run cross validation #######################
cat("\n\n#############   Running cross validation ###################")
crossValidationDfOrig = cbind(signals, yCV)
#correl =cor(crossValidationDfOrig)
#correl [!lower.tri(correl )]=0
#predictors = apply(abs(correl),1,max)
#predictors = names(crossValidationDfOrig)[predictors<0.8]
#crossValidationDfOrig = crossValidationDfOrig[,predictors,with=F]
crossValidationDfOrig[["volatility"]] = computeVolatility(dataCV)
crossValidationDfOrig[["buckets"]] <- cut(crossValidationDfOrig[["volatility"]], breaks = capLimits[["volatility"]], labels = seq(1, numBucketsVolatility))
#cols = c(names(crossValidationDf)[grep("mid", names(crossValidationDf))],"y")
#crossValidationDf = crossValidationDf[,cols,with=F]
modelPrediction = c()
actualReturn = c()
coefsTable = data.table(index=1:(ncol(crossValidationDfOrig)-3))
for (buck in seq(1,numBucketsVolatility)){
	cat(paste0("\n\n#############   Calibrating model for bucket ", buck, " ###################\n"))
	crossValidationDf = crossValidationDfOrig[buckets==buck,!"buckets"][,!"volatility"]
	cv_nfolds = 10
	cv_folds <- cut(seq(1,nrow(crossValidationDf)),breaks=cv_nfolds,labels=FALSE)
	squared_error <- data.table()
	pred_temp <- c()
	returns_temps <- c()
	lambda=NULL
	for(i in seq(1,cv_nfolds)){
		print(i)
		training <- crossValidationDf[cv_folds != i]
		valid <- crossValidationDf[cv_folds==i]
		model <- glmnet(as.matrix(training[,!"y"]), training$y, alpha = alphaElasticNet, lambda = lambda, standardize = FALSE, intercept=FALSE)
		lambda = model$lambda
		predictions = as.matrix(valid[, !"y"]) %*% as.matrix(coef(model)[-1,])
		#predictions = predict(model, as.matrix(valid[, !"y"]))
		error = predictions - valid$y
		pred_temp <- rbind(pred_temp,predictions)
		returns_temps <- c(returns_temps, valid$y)
		error2 = error^2
		squared_error = rbind(squared_error, data.table(error2))
	}
	res = apply(squared_error, 2, mean)
	std = apply(squared_error[seq(1,nrow(squared_error),300),], 2, sd) / nrow(squared_error[seq(1,nrow(squared_error),300),])
	bestLambda = lambda[which.min(res)]
	se1Lambda = max(lambda[res < min(res) + std[which.min(res)]])
	se1LambdaIdx = which(lambda==se1Lambda)
	rssList = 1 - res/ mean(returns_temps^2)
	percentLambda = max(lambda[rssList > max(rssList) - 0.001])
	percentLambdaIdx = which(lambda==percentLambda)
	
	#plot(lambda[20:length(lambda)], res[20:length(res)])
	#plot(lambda[40:length(lambda)], rssList[40:length(rssList)])
	
	rsquared   = 1 - res[percentLambdaIdx] / mean(returns_temps^2)
	coefs      = as.matrix(coef(model)[-1,])[,percentLambdaIdx]
	coefsTable[[buck]] = coefs
	modelPrediction=c(modelPrediction, as.matrix(pred_temp)[,percentLambdaIdx])
	actualReturn = c(actualReturn, returns_temps)
}
#plot(model$glmnet.fit, "norm",   label=TRUE)

#pred_temp = data.table(pred_temp)
#correl    = cor(pred_temp[[names(pred_temp)[which.min(res)]]], returns_temps)
#accuracy  = sum(pred_temp[[names(pred_temp)[which.min(res)]]] * returns_temps > 0) / sum(returns_temps!=0)
names(coefsTable) = sapply(seq(1, numBucketsVolatility), function(x) paste0("model",x))
rsquared = 1-var(modelPrediction-actualReturn) / var(actualReturn)
print(paste0("The explained variance on the test set is ",round(rsquared*100,2),"%."))

pred = data.table(as.matrix(crossValidationDfOrig[, !"y"]) %*% as.matrix(coefsTable))
coefs[order(abs(coefs),decreasing=T)]
test = lars(as.matrix(training[,!"y"]), training$y, type = "forward.stagewise", normalize = F, intercept = F)
################ compute statistics per bucket #################
finalPred = pred_temp[,percentLambdaIdx]
stats     = data.table(pred=finalPred, ret=returns_temps)
stats[,bucket:=cut(pred, breaks=c(quantile(pred, probs = seq(0, 1, by = 0.1))))]
stats[,.(c = cor(pred,ret)), by=bucket]
results   = stats[,.(correlation = cor(pred,ret), standarDevPred=sd(pred), standarDevRet=sd(ret), rsq=1-sum((pred-ret)^2)/sum(ret^2),accuracy=sum(pred*ret>0)/sum(ret!=0), count=length(pred)), by=bucket]
results[order(-rank(bucket))]

############### compute results on test set ####################
cat("\n\n#############  Checking generalization on test set ###################\n")
test_set = computeSignalsAndApplyLimits(dataTest, rangeAmounts, listLags, listEwma, capLimits, normalizationType)
test_set = cbind(test_set, yTest)
model <- glmnet(as.matrix(crossValidationDf[,!"y"]), crossValidationDf$y, alpha = alphaElasticNet, lambda = percentLambda, standardize = FALSE, intercept=FALSE)
finalPrediction = as.matrix(test_set[, !"y"]) %*% as.matrix(coef(model)[-1,])
#predictions = data.table(as.matrix(test_set[, !"y"]) %*% as.matrix(coefsTable))
#predictions[, volatility:=computeVolatility(dataTest)]
#predictions[["buckets"]] <- cut(predictions[["volatility"]], breaks = capLimits[["volatility"]], labels = seq(1, numBucketsVolatility))
#predictions[is.na(predictions)] = 1
#idx = predictions[["buckets"]]
#predictions= data.frame(predictions)
#predictions[["final"]] = predictions[cbind(seq_along(idx), idx)]
#finalPrediction = as.numeric(predictions[["final"]])
error = finalPrediction - test_set$y
error2 = error^2
varianceExplained = 1 - mean(error2) / mean(test_set$y^2)
correl = cor(finalPrediction,test_set$y)
print(paste0("The explained variance on the test set is ",round(varianceExplained*100,2),"%."))

############## recalibrate model on whole dataset ################
cat("\n\n#############  Recalibrating model on whole dataset ###################\n")
signals = computeSignalsAndApplyLimits(data, rangeAmounts, listLags, listEwma, capLimits)
model   = glmnet(as.matrix(signals), y$y, alpha = alphaElasticNet, lambda = bestLambda, standardize = FALSE, intercept=FALSE)
coefs   = as.matrix(coef(model)[-1,])
rm(list=c("data", "signals"))

############### for XTX : compute results on test dataset #########
cat("\n\n#############  Final test set   ###################")
testData = loadData("test_data.mat")
askRate  = testData[["askRate"]]
askSize  = testData[["askSize"]]
bidRate  = testData[["bidRate"]]
bidSize  = testData[["bidSize"]]
y        = testData[["y"]]
names(y) = c("y")
testData = mergeOBintoSingleDf(askRate, askSize, bidRate, bidSize)

signals = computeSignalsAndApplyLimits(testData, rangeAmounts, listLags, listEwma, capLimits)
predictions = as.matrix(signals) %*% coefs
varianceExplained = 1 - mean((predictions-y)^2) / mean(y^2)
print(paste0("The explained variance on the test set is ",round(varianceExplained*100,2),"%."))




