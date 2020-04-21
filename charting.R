library(quantmod)
library(TTR)
source("technicalIndicators.R") 
source("chartTools.R")
source("settings.R")
source("momFunction.R")

simpleChart <- function( stock, time='last 3 months'){
    chartSeries( stock,
                type="line",
                theme=chartTheme('white'),
                subset=time, 
                )
}

chartMACD <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    getMACD <- stockMACD( stock )
    macdIndicator <- indicatorMACD( zooToDataFrame( getMACD ) )
    buyDates <- macdIndicator$buy[ macdIndicator$buy > startDate ]
    buyXValues <- c()
    for( i in 1:length(buyDates) ){
        buyXValues[i] <- businessDayCounter(startDate, buyDates[i])
    }
    sellDates <- macdIndicator$sell[ macdIndicator$sell > startDate ]
    sellXValues <- c()
    for( i in 1:length(sellDates) ){
        sellXValues[i] <- businessDayCounter( startDate, sellDates[i])
    }
    ta <-addLinesToMACD(buysXValues, sellXValues) 
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=constructXtsDate(startDate, endDate),
            TA= ta
        )
}

chartRSI <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="RSI Chart 0.1" ){
    getRSI <-  stockEmaRSI( stock )
    rsiDF <- zooToDataFrame( getRSI ) 
    rsiSignal <- c() 
    for(i in 1:nrow(rsiDF) ){
        if( rsiDF$rsi[i] > rsiOverboughtConstant ){
            rsiSignal[i] = 1 
        } else if( rsiDF$rsi[i] < rsiOversoldConstant ){
            rsiSignal[i] = -1 
        } else {
            rsiSignal[i] = 0 
        }
    }
    rsiDF$signal <- rsiSignal
    #rsiDF$signal <- rsiSetup( rsiDF )
    limitDates <- indicatorRSI( rsiDF )  
    buyXValues <- c()
    #buyXValues <- rsiBuyXValues( limitDates$buy, startDate ) 
    buyDates <- limitDates$buy[ limitDates$buy > startDate ]
    for( i in 1:length(buyDates) ){
        buyXValues[i] <- businessDayCounter(startDate, buyDates[i])
    }
    sellXValues <- c()
    #sellXValues <- rsiBuyXValues( limitDates$sell, startDate ) 
    sellDates <- limitDates$sell[ limitDates$sell > startDate ]
    for( i in 1:length(sellDates) ){
        sellXValues[i] <- businessDayCounter( startDate, sellDates[i])
    }
    ta <-addLinesToRSI(buyXValues, sellXValues) 
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=constructXtsDate(startDate, endDate),
            TA= ta
        )
}

chartMomentum <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MOM Chart 0.1" ){
    getMOM <-  stockMomentum( stock )
    momDF <- zooToDataFrame( getMOM ) 
    momDF <- momentumParity( momDF )
    dateChange <- changeInSlopeDir( momDF ) 
    buyXValues <- momBuyXValues( dateChange$buy, startDate ) 
    sellXValues <- momSellXValues( dateChange$sell, startDate ) 
    ta <- addLinesToMOM(buyXValues, sellXValues) 
    print(ta)
    chartSeries(stock,
            name=cName,
            theme=chartTheme('white'),
            subset=constructXtsDate(startDate, endDate),
            TA= ta
        )

}    
chartBBands <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="BBands Chart 0.1" ){
    chartSeries( stock,
                 theme=chartTheme('white'),
                 subset='last 3 months',
                 TA="addBBands( n=20, sd=2 )"
    )
    
    
}
movingAverageChart <- function( stock, time='last 3 months'){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset=time, 
                TA="addSMA(13);addSMA(55)"
                )
}

#EMA(stock, lookbackPeriod) 
chartEMA <- function( stock, time='last 3 months'  ){
    chartSeries( stock,
                name=,
                subset='last 3 months',  
                theme=chartTheme('white'),
                TA="addEMA(13,on=1,col='blue');addEMA(55, on=1,col='red')"
                )
}


chartADX <- function( stock, time='last 3 months' ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset=time,
                TA="addADX()"
                )
}
