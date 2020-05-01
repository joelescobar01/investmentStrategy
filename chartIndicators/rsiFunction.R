library(TTR)
source("lib/utils.R")
source("var/settings.R")

stockSmaRSI <- function( stockDF, pastDays=14 ){
    rsi <- 
        RSI(Cl(stockDF), 
            SMA, n=pastDays)
    return( rsi) 
}
stockEmaRSI <- function( stockDF, pastDays=14 ){
    
    rsi <- 
        RSI(Cl(stockDF), 
            EMA, 
                n=pastDays)
    return( rsi) 
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
