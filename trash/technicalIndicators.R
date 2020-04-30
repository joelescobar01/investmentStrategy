library(TTR)
source("utils.R") 
source('settings.R')
source("rsiFunction.R")
Sys.setenv(TZ = "UTC")

stock3DayPeriod <- function( stock ){
    period.apply(CAT,endpoints(stock,on='days', k=4 ),
                 FUN=function(x) { max(Cl(x)) } ) 
}
stockWeeklyHighs <- function(stock){
    return( Hi( to.weekly(stock) ) )
}

stockMonthlyHighs <- function(stock){
    return( Hi( to.monthly(stock) ) )
}

stockWeeklyLows <- function(stock){
    return( Lo( to.weekly(stock) ) )
}

stockMonthlyLows <- function(stock){
    return( Lo( to.monthly(stock) ) )
}

lagDiff <- function( vect1, vect2, lag=1 ){
    diffV <- c() 
    for(i in 2:nrow( vect1 ) ){
        diffV[i] = sign(vect1[i]) - sign(vect2[i-lag]) 
    }
    return(diffV) 
}

buyLargePriceDiff <- function( stock, threshold=0.005 ){
    price <- Cl(stock) # close price
    r <- price/Lag(price) - 1 # % price change
    delta <- threshold #threshold
    signal <-c(0) # first date has no signal
    #Loop over all trading days (except the first)
    for (i in 2: length(price)){
        if (r[i] > delta){
            signal[i]<- 1
        } else
            signal[i]<- 0
    }
    print(signal)
    print(price)
    signal<-reclass(signal,price)
    return(signal)
}

weekHighs <- function( stock, week ){
   return( seriesHi( tail( to.weekly(stock), n=week ) ) )
}

returnsLog <- function(stock ){
  diff(OpCl(stock))
}

# weekLows <- function( stock, week ){
#   return( seriesHi( tail( to.weekly(CAT), n=week ) ) )
# }
# 
# weeksMaxVolumeTrade <- function( stock, week ){
#   weekRecords <- tail( to.weekly(stock,FUN=function(x) { max(Vo(x)) } ), n=week )
#   
# }

  
  

dailyCandleBodySize <- function( stockClose, stockOpen ){
    bodySize <- c()
    min = length(stockClose)
    for(t in 1:min){
        bodySize[t] <- abs(stockOpen[t] - stockClose[t])
    }  
   return(bodySize) 
}

dailyCandleSize <- function( stockLow, stockHigh ){
    candleSize <- c() 
    min = length(stockLow) 
    for(t in 1:min){
        candleSize[t] <- stockHigh[t] - stockLow[t] 
    }
    return(candleSize) 
}


