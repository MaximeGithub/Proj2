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
normalizationType = "standard"  #"standard" or "rolling"
rangeAmounts = c(5, 10, 15, 20, 30, 40, 50, 60, 70)
listLags = c(1, 10, 200)
listEwma = c(0, 10, 50, 100, 200, 500, 1000, 2000)
numBucketsVolatility = 1
alphaElasticNet = 0.9 # 1 is lasso

########### create validation and test sets ##############
folds <- cut(seq(1,nrow(data)),breaks=20,labels=FALSE)
capAndLimitFolds = 1:2
crossValidationFolds = 3:12
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

############# compute limits and signals ####################
cat("\n\n#############   Calibrating the limits ###################\n")
capLimits = calibrateCapAndFloorsForAllSignals(dataLimits, rangeAmounts, listLags, listEwma, numBucketsVolatility, normalizationType)
cat("\n\n#############   Computing signals for cross-validation ###################\n")
signals = computeSignalsAndApplyLimits(dataCV, rangeAmounts, listLags, listEwma, capLimits, normalizationType)

############ run cross validation #######################
cat("\n\n#############   Running cross validation ###################")
crossValidationDf = cbind(signals, yCV)
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
	error = predictions - valid$y
	pred_temp <- rbind(pred_temp,predictions)
	returns_temps <- c(returns_temps, valid$y)
	error2 = error^2
	squared_error = rbind(squared_error, data.table(error2))
}
res = apply(squared_error, 2, mean)
std = apply(squared_error[seq(1,nrow(squared_error),300),], 2, sd) / nrow(squared_error[seq(1,nrow(squared_error),300),])
rssList = 1 - res/ mean(returns_temps^2)
percentLambda = max(lambda[rssList > max(rssList) - 0.001])
percentLambdaIdx = which(lambda==percentLambda)

#plot(lambda[20:length(lambda)], res[20:length(res)])
rsquared   = 1 - res[percentLambdaIdx] / mean(returns_temps^2)
coefs      = as.matrix(coef(model)[-1,])[,percentLambdaIdx]
print(paste0("The explained variance on the test set is ",round(rsquared*100,2),"%."))

#coefs[order(abs(coefs),decreasing=T)]
#test = lars(as.matrix(training[,!"y"]), training$y, type = "forward.stagewise", normalize = F, intercept = F)

############### compute results on test set ####################
cat("\n\n#############  Checking generalization on test set ###################\n")
test_set = computeSignalsAndApplyLimits(dataTest, rangeAmounts, listLags, listEwma, capLimits, normalizationType)
test_set = cbind(test_set, yTest)
model <- glmnet(as.matrix(crossValidationDf[,!"y"]), crossValidationDf$y, alpha = alphaElasticNet, lambda = percentLambda, standardize = FALSE, intercept=FALSE)
finalPrediction = as.matrix(test_set[, !"y"]) %*% as.matrix(coef(model)[-1,])
error = finalPrediction - test_set$y
error2 = error^2
varianceExplained = 1 - mean(error2) / mean(test_set$y^2)
correl = cor(finalPrediction,test_set$y)
print(paste0("The explained variance on the test set is ",round(varianceExplained*100,2),"%."))

############## recalibrate model on whole dataset ################
cat("\n\n#############  Recalibrating model on whole dataset ###################\n")
signals = computeSignalsAndApplyLimits(data, rangeAmounts, listLags, listEwma, capLimits)
model   = glmnet(as.matrix(signals), y$y, alpha = alphaElasticNet, lambda = percentLambda, standardize = FALSE, intercept=FALSE)
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




