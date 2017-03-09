loadData = function(fileName, path = getwd()){
	### this function loads the matlab file
	#load dataframes
	askRate <- data.table(h5read(paste(path,fileName,sep="\\"), "/askRate"))
	askSize <- data.table(h5read(paste(path,fileName,sep="\\"), "/askSize"))
	bidRate <- data.table(h5read(paste(path,fileName,sep="\\"), "/bidRate"))
	bidSize <- data.table(h5read(paste(path,fileName,sep="\\"), "/bidSize"))
	y <- data.table(h5read(paste(path,fileName,sep="\\"), "/y"))
	#rename columns
	names(askRate) = sapply(1:ncol(askRate), function(x) sprintf("ask_price_%i", x))
	names(bidRate) = sapply(1:ncol(bidRate), function(x) sprintf("bid_price_%i", x))
	names(askSize) = sapply(1:ncol(askSize), function(x) sprintf("ask_size_%i", x))
	names(bidSize) = sapply(1:ncol(bidSize), function(x) sprintf("bid_size_%i", x))
	#returns data frames as list
	return(list("askRate" = askRate, "askSize" = askSize, "bidRate" = bidRate, "bidSize"=bidSize, "y"=y))
}

mergeOBintoSingleDf = function(df_askRate, df_askSize, df_bidRate, df_bidSize){
	df_data = cbind(df_askRate, df_askSize, df_bidRate, df_bidSize)
	return(df_data)
}