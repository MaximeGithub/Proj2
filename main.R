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
rangeAmounts = c(5, 20, 50, 100)
listLags = c(1)
listEwma = c(0, 10, 30)
alphaElasticNet = 1 # 1 is lasso

########### create validation and test sets ##############
folds <- cut(seq(1,nrow(data)),breaks=20,labels=FALSE)
capAndLimitFolds = c(1)
crossValidationFolds = 2:16
crossValidationFolds = 2:12 ################################################################ testing purpose only
testingFolds = 17:20

dataLimits = data[folds %in% capAndLimitFolds]
yLimits = y[folds %in% capAndLimitFolds]

dataCV = data[folds %in% crossValidationFolds]
yCV = y[folds %in% crossValidationFolds]

dataTest = data[folds %in% testingFolds]
yTest = y[folds %in% testingFolds]

############# compute limits and signals ####################
cat("\n\n#############   Calibrating the limits ###################\n")
capLimits = calibrateCapAndFloorsForAllSignals(dataLimits, rangeAmounts, listLags, listEwma)
cat("\n\n#############   Computing signals for cross-validation ###################\n")
signals = computeSignalsAndApplyLimits(dataCV, rangeAmounts, listLags, listEwma, capLimits)

##############test new signals
signals[is.na(signals)] = 0
signals[,test1:=diff_weighted_5_ewma_0 - diff_weighted_5_ewma_10][,test2:=diff_weighted_10_ewma_0 - diff_weighted_10_ewma_10][,test3:=diff_weighted_20_ewma_0 - diff_weighted_20_ewma_10]

basicSignals = createBasicSignals(dataCV, rangeAmounts, listLags)
basicSignals[is.na(basicSignals)] = 0
movingAverageSignals = computeMovingAverage(basicSignals, listEwma)
df = cbind(movingAverageSignals, yCV)
cols = c(names(df)[grep("diff_weighted", names(df))],names(df)[grep("diff_mid", names(df))], names(df)[grep("arrivalDeparture", names(df))],"y")
df2=df[,cols,with=F]
bidSize = dataCV[,c(names(dataCV)[grep("bid_size", names(dataCV))]),with=F]
askSize = dataCV[,c(names(dataCV)[grep("ask_size", names(dataCV))]),with=F]
askCumSize = data.table(t(apply(askSize, 1, cumsum)))
bidCumSize = data.table(t(apply(bidSize, 1, cumsum)))
newDf= (askCumSize - bidCumSize) / (askCumSize + bidCumSize)
df2=cbind(df2,newDf[,1:5])
df2[is.na(df2)]=0
#df2[,deltaDiff10:=diff_weighted_5_ewma_0 - diff_weighted_5_ewma_10][,deltaDiff50:=diff_weighted_10_ewma_0 - diff_weighted_5_ewma_50][,deltaDiff100:=diff_weighted_10_ewma_0 - diff_weighted_5_ewma_100]
mod = lm(y~.-1,data=df2)
mod2=glmnet(as.matrix(df2[,!"y"]), df2$y, alpha = 1, standardize = FALSE, intercept=FALSE)
summary(mod)

############ run cross validation #######################
cat("\n\n#############   Running cross validation ###################")
crossValidationDf = cbind(signals, yCV)
#cols = c(names(crossValidationDf)[grep("mid", names(crossValidationDf))],"y")
#crossValidationDf = crossValidationDf[,cols,with=F]
cv_nfolds = 10
cv_folds <- cut(seq(1,nrow(crossValidationDf)),breaks=cv_nfolds,labels=FALSE)
squared_error <- data.table()
pred_temp <- c()
returns_temps <- c()
lambda=seq(0,0.02,0.001)
lambda=NULL
for(i in seq(1,cv_nfolds)){
	print(i)
	training <- crossValidationDf[cv_folds!=i]
	valid <- crossValidationDf[cv_folds==i]
	model <- glmnet(as.matrix(training[,!"y"]), training$y, alpha = alphaElasticNet, lambda = lambda, standardize = FALSE, intercept=FALSE)
	lambda = model$lambda
	# predictions = as.matrix(valid[, !"returns"]) %*% as.matrix(coef(model)[-1,])
	predictions = predict(model, as.matrix(valid[, !"y"]))
	error = predictions - valid$y
	pred_temp <- rbind(pred_temp,predictions)
	returns_temps <- c(returns_temps, valid$y)
	error2 = error^2
	squared_error = rbind(squared_error, data.table(error2))
}
res = apply(squared_error, 2, mean)
plot(lambda, res)

bestLambda = lambda[which.min(res)]
rsquared   = 1 - res[which.min(res)] / mean(returns_temps^2)
coefs      = as.matrix(coef(model)[-1,])[,which.min(res)]

#plot(model$glmnet.fit, "norm",   label=TRUE)

pred_temp = data.table(pred_temp)
correl    = cor(pred_temp[[names(pred_temp)[which.min(res)]]], returns_temps)
accuracy  = sum(pred_temp[[names(pred_temp)[which.min(res)]]] * returns_temps > 0) / sum(returns_temps!=0)
print(paste0("The explained variance on the test set is ",round(rsquared*100,2),"%."))

################ compute statistics per bucket #################
finalPred = pred_temp[[names(pred_temp)[which.min(res)]]]
stats     = data.table(pred=finalPred, ret=returns_temps)
stats[,bucket:=cut(pred, breaks=c(quantile(pred, probs = seq(0, 1, by = 0.1))))]
stats[,.(c = cor(pred,ret)), by=bucket]
results   = stats[,.(correlation = cor(pred,ret), standarDevPred=sd(pred), standarDevRet=sd(ret), rsq=1-sum((pred-ret)^2)/sum(ret^2),accuracy=sum(pred*ret>0)/sum(ret!=0), count=length(pred)), by=bucket]
results[order(-rank(bucket))]

############### compute results on test set ####################
cat("\n\n#############  Checking generalization on test set ###################\n")
test_set = computeSignalsAndApplyLimits(dataTest, rangeAmounts, listLags, listEwma, capLimits)
test_set = cbind(test_set, yTest)
model <- glmnet(as.matrix(crossValidationDf[,!"y"]), crossValidationDf$y, alpha = alphaElasticNet, lambda = bestLambda, standardize = FALSE, intercept=FALSE)
predictions = as.matrix(test_set[, !"y"]) %*% as.matrix(coef(model)[-1,])
error = predictions - test_set$y
error2 = error^2
varianceExplained = 1 - mean(error2) / mean(test_set$y^2)
correl = cor(predictions,test_set$y)
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




