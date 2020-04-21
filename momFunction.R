library(TTR)
source("utils.R")
source("settings.R")
#Sys.setnev(TZ="UTC")

stockMomentum <- function( stock, pastDays=2 ){
    mom <- momentum(Cl(stock), n=pastDays)
    return( na.omit(mom)) 
}

momentumParity <- function( momentumDF ){
    momentumDF$sign <- sapply( momentumDF[,-1], numberParity )         
    return( momentumDF ) 
}

changeInSlopeDir <- function( momentumDF ){
    zeroLessMomentum <- subset( momentumDF, sign != 0 ) 
    prevValue = first(momentumDF$sign) 
    buySignal <- c() 
    buyIndex = 1
    sellSignal <- c()
    sellIndex = 1
    for(i in 2:nrow( zeroLessMomentum ) ){
        if( prevValue != zeroLessMomentum$sign[i] ){
            if( prevValue > zeroLessMomentum$sign[i] ){
                sellSignal[sellIndex] <- zeroLessMomentum$Date[i] - timeLagConstant
                sellIndex = sellIndex + 1 
            } else {
                buySignal[buyIndex] <- zeroLessMomentum$Date[i]  - timeLagConstant
                buyIndex = buyIndex + 1 
            }
        }
        prevValue = zeroLessMomentum$sign[i] 
    }
    
    class(buySignal ) <- "Date" 
    class(sellSignal ) <- "Date" 
    dateChange <- list( "buy" = buySignal, "sell" = sellSignal ) 
    return( dateChange )
}

momBuyXValues <- function(buydates, startdate ){
    buyXValues <- c()
    buydates <- buydates[ buydates > startdate ]
    for( i in 1:length(buydates) ){
        buyXValues[i] <- businessDayCounter(startdate, buydates[i])
    }
    return( buyXValues )
}

momSellXValues <- function(selldates, startdate ){
    sellXValues <- c() 
    selldates <- selldates[ selldates > startdate ]
    for( i in 1:length(selldates) ){
        sellXValues[i] <- businessDayCounter( startdate, selldates[i])
    }
    return( sellXValues ) 
}
