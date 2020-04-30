library(quantmod)
library(plotly)
library(ggplot2)
library(PerformanceAnalytics)

getStock <- function(symbol){
  data1 <- NULL                               # NULL data1
  data1 <- tryCatch(getSymbols(Symbols = toupper(symbol),  
                               src = "yahoo", 
                               auto.assign = FALSE),
                    error=function(e){})      # empty function for error handling
  if(is.null(data1)) return(NA) 
  # if data1 is still NULL go to next ticker             # if data1 is still NULL go to next ticker  
  #stock <- adjustOHLC( data1 )
  data1 <- adjustOHLC(data1,
              use.Adjusted = TRUE,
            )
  return( na.fill(data1, fill=0.00 ) )
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
  dat <- as.data.frame(coredata(stock),row.names=1 )
  dat$date <- index(stock)
  dat <- subset(dat, date >= first( index(stock)) )
  
  str <- sprintf("^%s\\.", name)
  names(dat) <- sub(str, "", names(dat))
  return( dat )
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

zooToDataFrame <- function( xtsObj ){
     return( data.frame(Date=as.Date(index(xtsObj)), xtsObj, check.names=FALSE, row.names=1) )
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