library(quantmod)
library(plotly)
library(ggplot2)
library(PerformanceAnalytics)

getStock <- function(name){
  stock <-
    getSymbols(name, auto.assign = F ) #access CLose and Open with stock$MOTS.Open
  return( stock )
}

removeNameFromColumn <- function( stock, name ){
  #names(stock) <- gsub("^.+\\.","",names(name))
}

dataFrameColLag <- function( colA, colB, lag=1 ){
  #colA(t) - colB(t-1)
  diffCol <- c() 
  for(i in 1:length(colA) ){
    if( i == 1){
      diffCol[i] = NA
      i = i+1 
    } 
    diffCol[i] <- colA[i] + colB[i-lag]
  }
  return(diffCol)
}

convertStockToDataFrame <- function( stock, name ){
  dat <- as.data.frame(coredata(stock))
  dat$date <- index(stock)
  dat <- subset(dat, date >= first( index(stock)) )
  
  str <- sprintf("^%s\\.", name)
  names(dat) <- sub(str, "", names(dat))
  return( dat )
}

getStockDF <- function( name ){
    try ( stock <- getStock( name ) )
    if( missing(stock) )
        return(NA)
    stockDf <- convertStockToDataFrame( stock, name ) 
    return(stockDf) 
}

dfToTimeSeries <- function( dataFrame, indexCol=ncol(dataFrame) ){
    return( xts(dataFrame[,-indexCol], order.by=as.Date(  dataFrame[,indexCol], "%m/%d/%Y"))) 
}

dfToTimeSeries2 <- function( dataFrameData, indexCol=dataFrameDate ){
  return( xts(dataFrameData, order.by=as.Date(  indexCol, "%m/%d/%Y"))) 
}

businessDayCounter <- function( fromDate, toDate ){
    if ( is.null(fromDate) || is.null(toDate) ) {
        return(0)
    }
    workDays = sum(!weekdays(seq(fromDate, toDate, "days")) %in% c("Saturday", "Sunday"))
    return(workDays) 
}

numberParity <- function( num ){
    indicator <- sign( num )
    return(indicator) 
}

initStock <- function(){
  name <- readline(prompt = "Enter Stock Symbol: ")
  stock <- getStock(name)
  stockDF <- convertStockToDataFrame( stock, name )
  stockList <- list( stock, stockDF, name )
  names(stockList) <- c("stocks", "stocksDF", "name")
  return( stockList )
}

zooToDataFrame <- function( xtsObj ){
     return( data.frame(Date=as.Date(index(xtsObj)), xtsObj, check.names=FALSE, row.names=NULL) )
}

exportStockCSV <- function(stock, filename){
    x<-data.frame(stock)
    x$date<-rownames(x)
    rownames(x)<-NULL
    file <- paste( getwd(), "/", filename, sep="")
    write.csv(x, file, row.names = TRUE)
    return(x)
}

getPeaks <- function( data, minValue=0 ){
  return( na.omit( findPeaks(data, thresh=minValue) ))
}

getValleys <- function( data, minValue=0){
  return( na.omit(findValleys(data, thresh=minValue)))  
}

getQuoteToday <- function(stock) {
    av_api_key("A3SZWB8GG47PAV8O")
    df <- av_get( symbol = stock,
            av_fun  = "TIME_SERIES_INTRADAY",
            interval = "15min",
            outputsize = "full") 
    return(df) 
}  

dailyMedianPrice <- function( stockClose, stockOpen ){
    median <- c()
    min = length(stockClose)  
    for(t in 1:min ){
        median[t] <- (stockClose[t]+stockOpen[t])/2 
    }
    return(median) 
}

dailyUpperShadow <- function( stockClose, stockOpen, stockHigh ){
    upperShadow <- c()
    min = length(stockClose)
    for(t in 1:min){
        upperShadow[t] <- stockHigh[t] - max(stockClose[t], stockOpen[t])
    }  
   return(upperShadow) 
}

dailyLowerShadow <- function( stockClose, stockOpen, stockLow ){
    upperShadow <- c()
    min = length(stockClose)
    for(t in 1:min){
        upperShadow[t] <- min(stockClose[t], stockOpen[t]) - stockLow[t]
    }  
   return(upperShadow) 
}

trendSlope <- function( stock, period=length(stock)){
    df <- coredata( stock )
    trend <- c(0)
    for(t in 2:period){
        trend[t] <- df[t] - df[t-1] 
    }
    rise <- sum(trend ) 
    slope <- rise/period

    return(slope)
}

periodicSlopes <- function( stock ){
    dfDay <- trendSlope( Cl(stock )) 
    dfWeek <- trendSlope( Cl(to.weekly(stock)) ) 
    dfMonth <- trendSlope( Cl(to.monthly(stock)) ) 
    
    wholeSet = length(stock)
    weeklySet = length( to.weekly(stock) ) 
    monthlySet = length( to.monthly(stock) )

    period <- c( dfDay, dfWeek, dfMonth ) 
    
    return(period)     
}

correlationSP500 <- function( stock, stockScale="^GSPC", 
                             toDate=as.Date(Sys.Date()), fromDate=as.Date( to-90 ) ){
    sp500 <- getSymbols(stockScale, 
                        from=fromDate,
                        to=toDate,
                        auto.assign=FALSE ) 
    data<-cbind( diff(log(Cl(sp500))), diff(log(Cl(stock)))) 
    chart.Correlation(data) 
}


techSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXT", toDate=to, fromDate=from ) 
}

energySectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXE", toDate=to, fromDate=from ) 
}

financialSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXM", toDate=to, fromDate=from ) 
}

healthcareSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXV", toDate=to, fromDate=from ) 
}

industrialSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXI", toDate=to, fromDate=from ) 
}

materialsSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXB", toDate=to, fromDate=from ) 
}

consumerSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXY", toDate=to, fromDate=from ) 
}

staplesSectorPerformance <- function( stock, to=as.Date(Sys.Date()), from=as.Date( to-90 )) {
    correlationSP500( stock, stockScale="IXR", toDate=to, fromDate=from ) 
}

trailingPriceVolatility <- function( stock, windowSize=2, lookBack=windowSize){ 
     
}

upwardPriceMovement <- function( stockClose, stockOpen){
    upward <- c()
    for( t in 1:length(stockClose) ){
        if( stockClose[t] > stockOpen[t] ){
            upward[t] = 1 
        } else {
            upward[t] = -1
        }
    }
    return(upward) 
}

plotTrend <- function( priceMove ){
    x <- c(0)
    y <- c(0)
    for(i in 2:length(priceMove)){
        x[i] <- i 
        y[i] <- y[i-1]+(x[i]*priceMove[i])
    }
    plot( x, y, type="s", xlab=NULL, ylab=NULL, main=NULL )

}

strongBullish <- function( stockClose ){
    periodClose <- c()
    for(t in seq(from=6, to=length(stockClose), by=5) ){
        vlen <- length(periodClose )+1
        periodClose[vlen] <- stockClose[t] 
    }

    return(periodClose) 
}
