library(TTR)
source("lib/utils.R")
source("var/settings.R")

stockBBands <- function( stock, sma=13, stdeviation=2 ){
    #where it is greater 1 if it is above the upper band, and less than 0 when it is below the lower band
    bb <- BBands( Cl( stock ), n=sma, sd=stdeviation ) 
    return( na.omit(bb)) 
}
aboveUpBand <- function( bbandDF ){
    aboveUpDates <- bbandDF[ bbandDF$pctB > 1,'Date' ]
    return( aboveUpDates )
} 


belowDownBand <- function( bbandDF ){
    belowDownDates <- bbandDF[ bbandDF$pctB < 0,'Date' ]
    return( belowDownDates )
}

bbSetup <- function( BBandDF ){ 
    sellSignal <- belowDownBand(BBandDF)
    buySignal <- aboveUpBand(BBandDF)
    
    class(buySignal) <- "Date"
    class(sellSignal) <- "Date"
    dateList <- list( "buy" = buySignal, "sell" = sellSignal ) 
    return( dateList ) 
}

bbandBuyXValues <- function( buyDates, startdate ){
    buyXValues <- c()
    buydates <- buyDates[ buyDates > startdate ]
    if(length(buydates) == 0 )
        return(c())
    for( i in 1:length(buydates) ){
        buyXValues[i] <- businessDayCounter(startdate, buydates[i])
    }
    return( buyXValues )
}

bbandSellXValues <- function( sellDates, startdate ){
    sellXValues <- c() 
    selldates <- sellDates[ sellDates > startdate ]
    if(length(selldates) == 0 )
        return(c())
    for( i in 1:length(selldates) ){
        sellXValues[i] <- businessDayCounter( startdate, selldates[i])
    }
    return( sellXValues ) 
}


