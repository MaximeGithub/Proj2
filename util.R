computeReturns <- function(price){
	# return (c(NA, diff(price)/price[-length(price)]))
	return (c(NA, diff(price)))
}

ewma<-function(x, halfTime = 1, init = x[1]) {	
	if (halfTime == 0){
		return(x) #special case : no moving average
	}
	lambda <- 1 - exp(-log(2) / halfTime)
	rval <- filter(lambda*x,filter=(1-lambda),method="recursive",init=init)
}

rollingFunction <- function(vec, width, FUN) {
	sapply(seq_along(vec), function(i) if (i < width) NA else FUN(vec[i:(i-width+1)]))
}

computeRSquared <- function(y, yHat){
	return(1-var(y-yHat) / var(y))
}

myVar <- function(name, genericF){
	calls <- paste(sapply(name, genericF), collapse=",")
	as.quoted(paste('c(',calls,')'))[[1]]
}

myfun <- function(name, genericF){
	calls <- paste(sapply(name, genericF), collapse=",")
	as.quoted(paste('list(',calls,')'))[[1]]
}

composeCols <- function(a, levs, fun, ...) {
	a[,eval(myfun(levs, fun)),...]
}