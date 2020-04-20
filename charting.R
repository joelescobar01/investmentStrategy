library(quantmod)
library(TTR)
source("technicalIndicators.R") 
source("chartTools.R")
source("settings.R")

simpleChart <- function( stock, time='last 3 months'){
    chartSeries( stock,
                type="line",
                theme=chartTheme('white'),
                subset=time, 
                )
}
candleStickChartTicks <- function( stock, cName="" ){
    l <- xts(!as.logical(stock[,1]),index(stock))
    l[ length(stock[,1]) ] <- TRUE
    chart_Series(stock,
                 name=cName,
                 TA="add_TA(l,on=-1,col='grey',border='grey')", )
}

chartMACD <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, cName="MACD Chart 0.1" ){
    getMACD <- stockMACD( stock )
    macdIndicator <- indicatorMACD( zooToDataFrame( getMACD ) )
    buyDates <- macdIndicator$buy[ macdIndicator$buy > startDate ]
    buyXValues <- c()
    for( i in 1:length(buyDates) ){
        buyXValues[i] <- sum(!weekdays(seq(startDate, buyDates[i], "days")) %in% c("Saturday", "Sunday"))
    }
    sellDates <- macdIndicator$sell[ macdIndicator$sell > startDate ]
    sellXValues <- c()
    for( i in 1:length(sellDates) ){
        sellXValues[i] <- sum(!weekdays(seq(startDate, sellDates[i], "days")) %in% c("Saturday", "Sunday"))
    }
    ta <- paste( "addMACD()", addVLinesToTSChart(buyXValues, c(1,2), buyIndicatorColor), addVLinesToTSChart(sellXValues, c(1,2), sellIndicatorColor), sep=";")
    chartSeries(stock,
                name=cName,
                theme=chartTheme('white'),
                subset=constructXtsDate(startDate, endDate),
                TA= ta
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

#BBands(Cl(), n=20, sd=2)
# Cl, AvgHi, H, L, Cl can be used 
chartBBands <- function( stock, from=20, sd=2, time='last 3 months'  ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset='last 3 months', 
                TA="addBBands( n=20, sd=2 )"
                )

}

chartMomentum <- function( stock, time='last 3 months'  ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset='last 3 months', 
                TA="addMomentum(n=1)"
                )
}

chartADX <- function( stock, time='last 3 months' ){
    chartSeries( stock,
                theme=chartTheme('white'),
                subset=time,
                TA="addADX(n=14, maType=EMA)"
                )
}
