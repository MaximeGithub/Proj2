computeWeightedPrice = function(df_rate, df_size, amount, fillValue = NA){
	df_cumSize = data.table(t(apply(df_size, 1, cumsum)))
	df_cumSize[df_cumSize>amount] = amount
	df_diffSize = cbind(df_cumSize[,1], df_cumSize[,-1,with=F] - df_cumSize[,-length(df_cumSize),with=F]) 
	result = rowSums(df_diffSize*df_rate, na.rm=T) / rowSums(df_diffSize, na.rm=T)
	result[rowSums(df_diffSize, na.rm=T)<amount] = fillValue
	return(result)
}

createWeightedPrice = function(df_bidRate, df_bidSize, df_askRate, df_askSize, list_amounts){
	print("Computing weighted prices")
	ptm = Sys.time()
	df_weightedPrices = data.table(bid_weighted_1=df_bidRate[["bid_price_1"]], ask_weighted_1=df_askRate[["ask_price_1"]])
	df_weightedPrices[,mid_weighted_1:=(bid_weighted_1+ask_weighted_1)/2]
	for (amount in list_amounts){
		print(paste("Compute weighted prices for amount",amount))
		df_weightedPrices[[paste0("bid_weighted_",amount)]] = computeWeightedPrice(df_bidRate, df_bidSize, amount)
		df_weightedPrices[[paste0("ask_weighted_",amount)]] = computeWeightedPrice(df_askRate, df_askSize, amount)
		df_weightedPrices[[paste0("mid_weighted_",amount)]] = (df_weightedPrices[[paste0("bid_weighted_",amount)]]+df_weightedPrices[[paste0("ask_weighted_",amount)]]) / 2
		df_weightedPrices[[paste0("diff_weighted_",amount)]] = df_weightedPrices[[paste0("mid_weighted_",amount)]] - df_weightedPrices[["mid_weighted_1"]]
	}
	cat(paste("Weighted prices computed in", as.numeric(round(difftime(Sys.time(), ptm,units="secs"),2)), "seconds.\n"))
	return(df_weightedPrices)
}

createPastReturns = function(df_askRate, df_bidRate, vector_lags){
	print("Computing past returns")
	ptm = Sys.time()
	vector_mid = df_askRate$ask_price_1 + df_bidRate$bid_price_1
	df_lags = data.table(index=1:length(vector_mid))
	for (lag in vector_lags){
		print(paste("Compute past returns for lag",lag))
		df_lags[[paste0("diff_mid_",lag)]] = c(rep(NA,lag),diff(vector_mid,lag))
	}
	df_lags[,index:=NULL]
	cat(paste("Past returns computed in", round(Sys.time() - ptm,2), "seconds.\n"))
	return(df_lags)	
}

createArrivalDepartureOld = function(df_askRate, df_askSize, df_bidRate, df_bidSize){
	print("Computing arrival departure signal")
	ptm = Sys.time()
	vector_ask 	   = df_askRate$ask_price_1
	vector_bid     = df_bidRate$bid_price_1
	vector_askSize = df_askSize$ask_size_1
	vector_bidSize = df_bidSize$bid_size_1
	#vector_shiftedAskSize = c(NA,vector_askSize[1:length(vector_askSize)])
	#vector_shiftedBidSize = c(NA,vector_bidSize[1:length(vector_bidSize)])
	diff_ask = c(rep(NA,1),diff(vector_ask,1))
	diff_bid = c(rep(NA,1),diff(vector_bid,1))
	diff_ask_size = c(rep(NA,1),diff(vector_askSize,1))
	diff_bid_size = c(rep(NA,1),diff(vector_bidSize,1))
	ask_arr = ((diff_ask < 0) | (diff_ask_size > 0 & diff_ask == 0))*1
	ask_dep = ((diff_ask > 0) | (diff_ask_size < 0 & diff_ask == 0))*1
	bid_arr = ((diff_bid > 0) | (diff_bid_size > 0 & diff_bid == 0))*1
	bid_dep = ((diff_bid < 0) | (diff_bid_size < 0 & diff_bid == 0))*1
	#ask_arr = ifelse(diff_ask < 0, vector_askSize, ifelse(diff_ask_size > 0 & diff_ask == 0,diff_ask_size, 0))
	#ask_dep = ifelse(diff_ask > 0, vector_shiftedAskSize,ifelse(diff_ask_size < 0 & diff_ask == 0, -diff_ask_size, 0))
	#bid_arr = ifelse(diff_bid > 0, vector_bidSize, ifelse(diff_bid_size > 0 & diff_bid == 0,diff_bid_size,0))
	#bid_dep = ifelse(diff_bid < 0, vector_shiftedBidSize, ifelse(diff_bid_size < 0 & diff_bid == 0, -diff_bid_size,0))
	arrival_departure = - (ask_arr - ask_dep - bid_arr + bid_dep)
	cat(paste("Arrival departure signal computed in", round(Sys.time() - ptm,2), "seconds.\n"))
	return (data.table(arrivalDeparture = arrival_departure))
}


createArrivalDeparture = function(df_askRate, df_askSize, df_bidRate, df_bidSize){
	print("Computing arrival departure signal")
	ptm = Sys.time()
	#df_na = data.table(matrix(NA, nrow = 1, ncol = ncol(df_bidSize)))
	
	#df_askSize[is.na(df_askSize)] = 0
	#df_bidSize[is.na(df_bidSize)] = 0
	#df_askRate[is.na(df_askRate)] = Inf
	#df_bidRate[is.na(df_bidRate)] = 0
	
	#df_previousAskSize = rbindlist(list(data.table(matrix(NA, nrow = 1, ncol = ncol(df_askSize))),df_askSize[2:nrow(df_askSize)]),use.names=F, fill=F, idcol=NULL)
	#df_previousBidSize = rbindlist(list(data.table(matrix(NA, nrow = 1, ncol = ncol(df_bidSize))),df_bidSize[2:nrow(df_bidSize)]),use.names=F, fill=F, idcol=NULL)
	
	df_diffBidSize = rbindlist(list(data.table(matrix(NA, nrow = 1, ncol = ncol(df_bidSize))),df_bidSize[2:nrow(df_bidSize)] - df_bidSize[1:(nrow(df_bidSize)-1)]),use.names=F, fill=F, idcol=NULL)
	names(df_diffBidSize) = names(df_bidSize)
	df_diffAskSize = rbindlist(list(data.table(matrix(NA, nrow = 1, ncol = ncol(df_askSize))),df_askSize[2:nrow(df_askSize)] - df_askSize[1:(nrow(df_askSize)-1)]),use.names=F, fill=F, idcol=NULL)
	names(df_diffAskSize) = names(df_askSize)
	df_diffBidRate = rbindlist(list(data.table(matrix(NA, nrow = 1, ncol = ncol(df_bidRate))),df_bidRate[2:nrow(df_bidRate)] - df_bidRate[1:(nrow(df_bidRate)-1)]),use.names=F, fill=F, idcol=NULL)
	names(df_diffBidRate) = names(df_bidRate)
	df_diffAskRate = rbindlist(list(data.table(matrix(NA, nrow = 1, ncol = ncol(df_askRate))),df_askRate[2:nrow(df_askRate)] - df_askRate[1:(nrow(df_askRate)-1)]),use.names=F, fill=F, idcol=NULL)
	names(df_diffAskRate) = names(df_askRate)
	
	ask_arr = ((df_diffAskRate < 0) | (df_diffAskSize > 0 & df_diffAskRate == 0))*1
	ask_dep = ((df_diffAskRate > 0) | (df_diffAskSize < 0 & df_diffAskRate == 0))*1
	bid_arr = ((df_diffBidRate > 0) | (df_diffBidSize > 0 & df_diffBidRate == 0))*1
	bid_dep = ((df_diffBidRate < 0) | (df_diffBidSize < 0 & df_diffBidRate == 0))*1
	
	#ask_arr = ((df_diffAskRate < 0) *  df_askSize) + (df_diffAskSize > 0 & (df_diffAskRate == 0)) * df_diffAskSize
	#ask_dep = ((df_diffAskRate > 0) *  df_previousAskSize) - (df_diffAskSize < 0 & (df_diffAskRate == 0)) * df_diffAskSize
	#bid_arr = ((df_diffBidRate > 0) *  df_bidSize) + (df_diffBidSize > 0 & (df_diffBidRate == 0)) * df_diffBidSize
	#bid_dep = ((df_diffBidRate < 0) *  df_bidSize) - (df_diffBidSize < 0 & (df_diffBidRate == 0)) * df_diffBidSize
	
	arrival_departure = data.table(- (ask_arr - ask_dep - bid_arr + bid_dep))
	arrival_departure[is.na(arrival_departure)] = 0
	#arrival_departure = data.table(t(apply(arrival_departure, 1, cumsum)))
	names(arrival_departure) = sapply(seq(1,ncol(arrival_departure)),function(x) paste0("arrival_departure_level",x))
	arrival_departure = arrival_departure[,c(1,4,8)]
	cat(paste("Arrival departure signal computed in", round(Sys.time() - ptm,2), "seconds.\n"))
	return (data.table(arrivalDeparture = arrival_departure))
}

createfirstLevelImbalance = function(df_askRate, df_askSize, df_bidRate, df_bidSize){
	print("Computing first level imbalance")
	ptm = Sys.time()
	vector_ask 	   = df_askRate$ask_price_1
	vector_bid     = df_bidRate$bid_price_1
	vector_askSize = df_askSize$ask_size_1
	vector_bidSize = df_bidSize$bid_size_1
	obImbalance = (vector_ask * vector_askSize + vector_bid * vector_bidSize) / (vector_askSize + vector_bidSize) - (vector_ask + vector_bid) / 2
	cat(paste("First level imbalance computed in", round(Sys.time() - ptm,2), "seconds.\n"))
	return (data.table(obImbalance = obImbalance))
}

createVolumeImbalance = function(df_askSize, df_bidSize){
	print("Computing volume imbalance")
	ptm = Sys.time()
	askCumSize = data.table(t(apply(df_askSize, 1, cumsum)))
	bidCumSize = data.table(t(apply(df_bidSize, 1, cumsum)))
	askCumSize[is.na(askCumSize)] = 0
	bidCumSize[is.na(bidCumSize)] = 0
	df_imbalance = -(askCumSize - bidCumSize) / (askCumSize + bidCumSize)
	df_imbalance=df_imbalance[,1:5]
	df_imbalance[is.na(df_imbalance)]=0
	names(df_imbalance) = sapply(seq(1,5), function(x) paste0("volume_imbalance_", x))
	cat(paste("Volume imbalance computed in", round(Sys.time() - ptm,2), "seconds.\n"))
	return (df_imbalance)
}

applyLimitsToWeightedPrices = function(df_weightedPrices, list_limits){
	cols = c(names(df_weightedPrices)[grep("diff", names(df_weightedPrices))])
	df_weightedPrices = capFloorColumns(df_weightedPrices, cols, list_limits)
	for (column in cols){
		limits = list_limits[column][[1]]
		if ((length(limits) != 2) | (sum(is.na(limits)) > 0)){
			continue
		}
		newCol = df_weightedPrices[[column]]
		splitCol = strsplit(column, "_")[[1]]
		hor = as.numeric(splitCol[length(splitCol)])
		idx1 =  !is.na(df_weightedPrices[[paste0("bid_weighted_",hor)]] ) & is.na(df_weightedPrices[[paste0("ask_weighted_",hor)]])
		idx2 =  is.na(df_weightedPrices[[paste0("bid_weighted_",hor)]] ) & !is.na(df_weightedPrices[[paste0("ask_weighted_",hor)]])
		newCol[idx1] = limits[2]
		newCol[idx2] = limits[1]
		df_weightedPrices[[column]] = newCol
	}
	return(df_weightedPrices)
}


computeLimits <- function(DT, columns, limits){
	listLimits <- list()
	for (col in columns){
		q = quantile(DT[[col]][is.finite(DT[[col]])], limits, na.rm=T)
		listLimits[col] = list(q)
	}
	return(listLimits)
}

capFloorColumns <- function(DT, columns, listLimits){
	for (col in columns){
		limits = listLimits[col][[1]]
		if ((length(limits) != 2) | (sum(is.na(limits)) > 0)){
			next
		}
		newCol = DT[[col]]
		newCol[newCol<limits[1]] = limits[1]
		newCol[newCol>limits[2]] = limits[2]
		DT[[col]] = newCol
	}
	return(DT)
}


calibrateCapAndFloorsForAllSignals = function(df_data, vector_rangeAmounts, vector_lags, vector_ewma, numBucketsVolatility, str_normalizationType){
	basicSignals = createBasicSignals(df_data, vector_rangeAmounts, vector_lags)
	cappedBasicSignals = capAndFloorBasicSignals(basicSignals)
	movingAverageSignals = computeMovingAverage(cappedBasicSignals, vector_ewma)
	if (str_normalizationType == "standard") {list_stdev = computeStandardDev(movingAverageSignals)}
	normalizedSignals = normalizeSignals(movingAverageSignals, str_normalizationType, list_stdev)
	signalsLimits = computeLimits(normalizedSignals, names(normalizedSignals), c(0.01, 0.99))
	capSignals = capFloorColumns(normalizedSignals, names(normalizedSignals), signalsLimits)
	capSignals[is.na(capSignals)] = 0
	listLimits = getLimitsFromFinalSignals(cappedBasicSignals, capSignals)
	volatilityBuckets = computeVolatilityBuckets(df_data, numBucketsVolatility)
	listLimits["volatility"] = list(volatilityBuckets)
	if (str_normalizationType == "standard") {listLimits["stdev"] = list(list_stdev)}
	return(listLimits)
}

createBasicSignals = function(df_data, vector_rangeAmounts, vector_lags){
	df_bidRate = df_data[,c(names(df_data)[grep("bid_price", names(df_data))]),with=F]
	df_askRate = df_data[,c(names(df_data)[grep("ask_price", names(df_data))]),with=F]
	df_bidSize = df_data[,c(names(df_data)[grep("bid_size", names(df_data))]),with=F]
	df_askSize = df_data[,c(names(df_data)[grep("ask_size", names(df_data))]),with=F]
	weightedPrices = createWeightedPrice(df_bidRate, df_bidSize, df_askRate, df_askSize, vector_rangeAmounts)
	pastReturns = createPastReturns(df_askRate, df_bidRate, vector_lags)
	arrivalDeparture = createArrivalDeparture(df_askRate, df_askSize, df_bidRate, df_bidSize)
	#firstLevelImbalance = createfirstLevelImbalance(df_askRate, df_askSize, df_bidRate, df_bidSize)
	volumeImbalance = createVolumeImbalance(df_askSize, df_bidSize)
	signals = cbind(weightedPrices, pastReturns, arrivalDeparture, volumeImbalance)
	return(signals)
}

capAndFloorBasicSignals = function(df_basicSignals){
	weightedPrices = df_basicSignals[,c(names(df_basicSignals)[grep("weighted", names(df_basicSignals))]),with=F]
	weightedPricesLimits = computeLimits(weightedPrices, names(weightedPrices), c(0.01, 0.99))
	capWeightedPrices = applyLimitsToWeightedPrices(weightedPrices, weightedPricesLimits)
	
	otherCols = names(df_basicSignals)[! names(df_basicSignals) %in% names(weightedPrices)]
	otherSignals = df_basicSignals[,otherCols,with=F]
	otherSignalsLimits = computeLimits(otherSignals, names(otherSignals), c(0.01, 0.99))
	capOtherSignals = capFloorColumns(otherSignals, names(otherSignals), otherSignalsLimits)
	
	cols_weightedPrices = c(names(weightedPrices)[grep("diff", names(weightedPrices))])
	signals = cbind(capWeightedPrices[,cols_weightedPrices,with=F], capOtherSignals)
	signals[is.na(signals)] = 0
	return(signals)
}

normalizeSignals = function(df_cappedBasicSignals, str_normalizationType, list_stdev){
	cols = names(df_cappedBasicSignals)
	df_normalizedSignals = df_cappedBasicSignals
	if (str_normalizationType == "rolling") {
		for (d in cols){
			stdev = runSD(df_normalizedSignals[[d]], 1000)
			df_normalizedSignals[[d]]= df_normalizedSignals[[d]] / stdev
		}
	} else if (str_normalizationType == "standard"){
		for (d in cols){
			stdev = list_stdev[[d]]
			df_normalizedSignals[[d]]= df_normalizedSignals[[d]] / stdev
		}
	}
	return(df_normalizedSignals)
}


getLimitsFromFinalSignals = function(df_cappedBasicSignals, df_finalSignals){
	listLimits <- list()
	listLimits[["basic"]] = list()
	listLimits[["normalized"]] = list()
	for (col in names(df_cappedBasicSignals)){
		listLimits[["basic"]][[col]] = range(df_cappedBasicSignals[[col]])
	}
	for (col in names(df_finalSignals)){
		listLimits[["normalized"]][[col]] = range(df_finalSignals[[col]])
	}
	return(listLimits)
}


capSignalsFromLimits = function(df_basicSignals, list_limits){
	weightedPrices = df_basicSignals[,c(names(df_basicSignals)[grep("weighted", names(df_basicSignals))]),with=F]
	capWeightedPrices = applyLimitsToWeightedPrices(weightedPrices, list_limits)
	
	otherCols = names(df_basicSignals)[! names(df_basicSignals) %in% names(weightedPrices)]
	otherSignals = df_basicSignals[,otherCols,with=F]
	capOtherSignals = capFloorColumns(otherSignals, names(otherSignals), list_limits)
	
	cols_weightedPrices = c(names(weightedPrices)[grep("diff", names(weightedPrices))])
	signals = cbind(capWeightedPrices[,cols_weightedPrices,with=F], capOtherSignals)
	signals[is.na(signals)] = 0
	return(signals)
}

computeSignalsAndApplyLimits = function(df_data, vector_rangeAmounts, vector_lags, vector_ewma, list_limits, str_normalizationType){
	basicSignals = createBasicSignals(df_data, vector_rangeAmounts, vector_lags)
	cappedBasicSignals = capSignalsFromLimits(basicSignals, list_limits[["basic"]])
	list_stdev = list()
	movingAverageSignals = computeMovingAverage(cappedBasicSignals, vector_ewma)
	if(str_normalizationType=="standard") {list_stdev =  list_limits[["stdev"]]}
	normalizedSignals = normalizeSignals(movingAverageSignals, str_normalizationType, list_stdev)
	capSignals = capFloorColumns(normalizedSignals, names(normalizedSignals), list_limits[["normalized"]])
	capSignals[is.na(capSignals)] = 0
	#return(movingAverageSignals) ##############careful
	return(capSignals)
}

computeMovingAverage = function(df_signals, vector_ewma){
	df_result = data.table(index = 1:nrow(df_signals))
	for (halfTime in vector_ewma){
		for (col in names(df_signals)){
			if (halfTime == 0) {
				df_result[[paste0(col,"_ewma_", halfTime)]] = df_signals[[col]]
			} else {
				df_result[[paste0(col,"_ewma_", halfTime)]] = df_signals[[col]] - ewma(df_signals[[col]], halfTime = halfTime)
				df_result[[paste0(col,"_ewma_", halfTime)]] = ewma(df_signals[[col]], halfTime = halfTime)
			}
		}
	}
	df_result[,index:=NULL]
	return(df_result)
}

computeVolatilityBuckets = function(df_data, numBucketsVolatility){
	volatility = computeVolatility(df_data)
	q = quantile(volatility,seq(0,1, 1/numBucketsVolatility),na.rm=T)
	return(q)
}

computeVolatility = function(df_data){
	df_askRate = df_data[,c(names(df_data)[grep("ask_price", names(df_data))]),with=F]
	df_bidRate = df_data[,c(names(df_data)[grep("bid_price", names(df_data))]),with=F]
	mid = (df_bidRate[["bid_price_1"]] + df_askRate[["ask_price_1"]]) / 2
	return = c(NA, diff(mid))
	volatility = runSD(return, 500)
	return(volatility)
}

computeStandardDev = function(df_signals){
	list_stdev = list()
	cols = names(df_signals)
	for (d in cols){
		stdev = sd(df_signals[[d]])
		list_stdev[[d]] = stdev
	}
	return(list_stdev)
}