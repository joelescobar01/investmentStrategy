library(TTR)
source("utils.R")
source("settings.R")
#Sys.setnev(TZ="UTC")

stockSmaRSI <- function( stock, pastDays=14 ){
    rsi <- RSI(Cl(stock), SMA, n=pastDays)
    return( na.omit(rsi)) 
}
stockEmaRSI <- function( stock, pastDays=14 ){
    rsi <- RSI(Cl(stock), EMA, n=pastDays)
    return( na.omit(rsi)) 
}

rsiSetup <- function(rsiDataFrame){
    rsiSignal <- c() 
    for(i in 1:nrow(rsiDataFrame) ){
        if( rsiDataFrame$rsi[i] > rsiOverboughtConstant ){
            rsiSignal[i] = 1 
        } else if( rsiDataFrame$rsi[i] < rsiOversoldConstant ){
            rsiSignal[i] = -1 
        } else {
            rsiSignal[i] = 0 
        }
    }
    return( rsiSignal ) 
}

rsibuyxvalues <- function(buydates, startdate ){
    buyxvalues <- c()
    buydates <- buydates[ buydates > startdate ]
    for( i in 1:length(buydates) ){
        buyxvalues[i] <- businessdaycounter(startdate, buydates[i])
    }
    return( buyxvalues )
}

rsisellxvalues <- function(selldates, startdate ){
    sellxvalues <- c() 
    selldates <- selldates[ selldates > startdate ]
    for( i in 1:length(selldates) ){
        sellxvalues[i] <- businessdaycounter( startdate, selldates[i])
    }
    return( sellxvalues ) 
}
